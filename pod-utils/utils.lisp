
;;; Copyright (c) 2004 Peter Denno
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without restriction,
;;; including without limitation the rights to use, copy, modify,
;;; merge, publish, distribute, sublicense, and/or sell copies of the
;;; Software, and to permit persons to whom the Software is furnished
;;; to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR
;;; ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
;;; CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;-----------------------------------------------------------------------

;;;
;;; Peter Denno
;;;  Date: 12/2/95 - on going.
;;;
;;; Generally applicable utilities. Some from Norvig's "Paradigms of
;;; Artificial Programming," Some from Kiczales et. al. "The Art of the
;;; Metaobject Protocol," some from Graham's "On Lisp," some from Sam Steingold.
;;;
(in-package :cl-user)

(defpackage pod-utils
  (:nicknames pod)
  (:use cl)
  (:export combinations flatten kintern sintern mapappend pairs memo debug-memo memoize 
	   clear-memoize defun-memoize VARS mac mac2 load-ht when-bind if-bind when-bind* 
	   substring remove-extra-spaces break-line-at read-string-to-list split 
	   name2initials c-name2lisp lisp-name2c single-p mklist longer group prune find2 before 
	   duplicate split-if mvs dbind decode-time-interval strcat tree-search depth-first-search 
	   prepend breadth-first-search update with-stack-size pprint-without-strings chop setx
	   reuse-cons intersect-predicates
	   defmemo system-clear-memoized-fns system-add-memoized-fn system-list-memoized-fns
	   system-forget-memoized-fns fail))
; ph: removed last1 new-reslist reslist-pop reslist-push reslist-fillptr now with-gensyms


(in-package :pod-utils)

;;; Purpose: Return the combinations possible when selecting one item
;;;          from each of the argument sets.
;;;         Example: (combinations '(a) '(b c) '(d e))
;;;                   => ((A B D) (A B E) (A C D) (A C E))
;;; Arg: sets - lists
;;; Value: a list of lists. If the argument is nil, it returns nil.
(defun combinations (&rest sets)
  (cond ((null sets) nil)
	(t 
	 (flet ((combinations-aux (aset bset)
		  (cond ((not aset) bset)
			((not bset) aset)
			(t (loop for a in aset
				 append (loop for b in bset
					      collect (list a b)))))))
	   (loop for set in (reduce #'combinations-aux sets)
		 collect (flatten set))))))

(defun flatten (input &optional accumulator)
  "Return a flat list of the atoms in the input.
   Ex: (flatten '((a (b (c) d))) => (a b c d))"
  (cond ((null input) accumulator)
	((atom input) (cons input accumulator))
	(t (flatten (first input)
		    (flatten (rest input) accumulator)))))

(declaim (inline kintern))
(defun kintern (string &rest args)
  "Apply FORMAT to STRING and ARGS, upcase the resulting string and
 intern it into the KEYWORD package."
  (intern (string-upcase (apply #'format nil (string string) args))
	  (find-package "KEYWORD")))

(declaim (inline sintern))
(defun sintern (string &rest args)
  "Apply FORMAT to STRING and ARGS, upcase the resulting string and
 intern it into the current (*PACKAGE*) package."
  (intern (string-upcase (apply #'format nil (string string) args))))

(defun mapappend (fun &rest args)
  (loop until (some #'null args)
	append (apply fun (loop for largs on args
				collect (pop (first largs))))))

(defun mapnconc (fun &rest args)
  (loop until (some #'null args)
	nconc (apply fun (loop for largs on args
				collect (pop (first largs))))))

;;; Purpose: Return a list of pairs of elements from the argument list:
;;; Ex: (pairs '(a b c d)) => ((a b) (a c) (a d) (b c) (b d) (c d))
;;;
;;; Args: inlist - a list
(defun pairs (inlist)
  (loop for sublist on inlist
	while (cdr sublist)
	append
	(loop for elem in (cdr sublist)
	      collect `(,(first sublist) ,elem))))

;;; Purpose: Called by memoize, below. This returns
;;;          the memoized function. Norvig, Page 270.
;;; When you want to use this on &rest args use :test #'equal :key #'identity
;;; Args: fn - the function object.
;;;       name - the function symbol.
;;;       key - On what argument the result is indexed.
;;;       test - Either eql or equal, the :test of the hash table.
(defun memo (fn name key test)
  "Return a memo-function of fn."
  (let ((table (make-hash-table :test test)))
    (setf (get name 'memo) table)
    #'(lambda (&rest args)
	(let ((k (funcall key args)))
	  (multiple-value-bind (val found-p)
	      (gethash k table)
	    (if found-p
		val
	      (setf (gethash k table) (apply fn args))))))))

(defun debug-memo (fn name key test)
  "Like memo but prints *hit* on every hit."
  (let ((table (make-hash-table :test test)))
    (setf (get name 'memo) table)
    #'(lambda (&rest args)
	(let ((k (funcall key args)))
	  (multiple-value-bind (val found-p)
	      (gethash k table)
	    (if found-p
		(progn (princ " *HIT*") val)
	      (progn
		(princ " *miss*")
		(setf (gethash k table) (apply fn args)))))))))

;;; Purpose: memoize the argument function.
;;; Arguments as those in memo.
(defun memoize (fn-name &key (key #'first) (test #'eql) (debug nil))
  "Replace fn-name's global definition with a memoized version."
	  #-Allegro-V4.3 (format t "~%;;; Memoizing (~a) ~a ****" test fn-name)
	  #+Allegro-V4.3 (format t "~%;;; Memoizing ~a ****" fn-name)
  (if debug
      (setf (symbol-function fn-name)
	    (debug-memo (symbol-function fn-name) fn-name key test))
    (setf (symbol-function fn-name)
	  (memo (symbol-function fn-name) fn-name key test))))

;;; Clear the hash table from the function.
(defun clear-memoize (fn-name)
  "Clear the hash table from a memo function."
  (let ((table (get fn-name 'memo)))
    (when table (clrhash table))))

;;; Purpose: define a function and memoize it.
;;; Limitations: only useful for default arguments, i.e.,
;;;              key on first and test eql. In all other
;;;              cases call (memoize <fn> :key <key> :test <test>).
(defmacro defun-memoize (fn args &body body)
  `(memoize (defun ,fn ,args ,body)))

;;; Stuff to use when you have a serious number of memoized functions,
;;; and you have a notion of "starting over." 
(defmacro defmemo (fname &body body)
  `(progn (defun ,fname ,@body)
     (eval-when (:load-toplevel)
       (memoize ',fname)
       (system-add-memoized-fn ',fname))))

(let ((+memoized-fns+ nil))
  (defun system-clear-memoized-fns ()
    (mapcar #'(lambda (x) 
                (warn "Clearing memoized ~A" x) 
                (clear-memoize x))
            +memoized-fns+))
  (defun system-add-memoized-fn (fname)
    (pushnew fname +memoized-fns+))
  (defun system-list-memoized-fns ()
    +memoized-fns+)
  (defun system-forget-memoized-fns ()
    (setf +memoized-fns+ nil))
)

;;; Purpose: Diagnostic (From Howard Stearns) that does
;;; (vars a b c) => (FORMAT *TRACE-OUTPUT* "~&a = ~S b = ~S c = ~S ~%" A B C)
(defmacro VARS (&rest variables)
  `(format *trace-output*
           ,(loop with result = "~&"
                  for var in variables
                  do
                  (setf result
                        (if (and (consp var)
                                 (eq (first var) 'quote))
                            (concatenate 'string result " ~S ")
                          (concatenate 'string result (string-downcase var) " = ~S ")))
                  finally (return (concatenate 'string result "~%")))
           ,@variables))

;;; The most essential macro building tool.
(defmacro mac (macro)
  `(pprint (macroexpand-1 ',macro)))

;;; Similar, but used on 'subtype' macros. 
(defmacro mac2 (macro)
  `(pprint (macroexpand-1 (macroexpand-1 ',macro))))

;;; Dirk H.P. Gerrits' "Lisp Code Walker" slides, ALU Meeting, Amsterdam, 2003. 
;;; With additional corrections (beyond that in his notes). 
(defvar *mea-hooks* (make-hash-table :test #'eq))
(defun macroexpand-all (form &optional env)
  "Macroexpand FORM recursively until none of its subforms can be further expanded."
  (multiple-value-bind (expansion macrop)
      (macroexpand-1 form env)
    (declare (ignore macrop))
    (let* ((key (and (consp form) (car form)))
           (hook (gethash key *mea-hooks*)))
      (cond (hook (funcall hook form env))
            ((and (consp form) (symbolp (car form)) (macro-function (car form)))
             (macroexpand-all expansion env))
            ((consp form) (cons (car form)
                                (mapcar #'(lambda (arg)
                                            (macroexpand-all arg env))
                                        (cdr form))))
            (t expansion)))))

(defun load-ht (ht key-value-pairs)
  "Load the argument hash table with the argument values
   provided in a flat list of <key> <value>. "
  (loop while key-value-pairs
	do
	(setf (gethash (pop key-value-pairs) ht)
	      (pop key-value-pairs)))
  ht)
  
(defmacro when-bind ((var expr) &body body)
  "Paul Graham ON LISP pg 145. when+let"
  `(let ((,var ,expr))
     (when ,var
       ,@body)))

(defmacro if-bind ((var expr) then else)
  `(let ((,var ,expr))
     (if ,var
         ,then 
       ,else)))

(defmacro when-bind* (binds &body body)
  "Paul Graham ON LISP pg 145. when+let*"
  (if (null binds)
      `(progn ,@body)
    `(let (,(car binds))
       (if ,(caar binds)
	   (when-bind* ,(cdr binds) ,@body)))))

(defmacro with-gensyms (syms &body body)
  "Paul Graham ON LISP pg 145. Used in macros to avoid variable capture."
  `(let ,(mapcar #'(lambda (s) 
		     `(,s (gensym)))
	  syms)
     ,@body))

(declaim (inline substring))
(defun substring (str1 str2)
  "Returns the place in str1 where str2 begins or nil if str2 is not in str1"
  (search str2 str1 :test #'string=))

(defun remove-extra-spaces (string)
  "Leave only one space between non-space characters of argument string."
  (let* ((len (length string))
         (new-string (make-array len :element-type 'character :fill-pointer 0)))
    (vector-push (char string 0) new-string)
    (loop for i from 1 to (1- len)
          unless (and (char= #\Space (char string i))
                      (char= #\Space (char string (1- i))))
          do (vector-push (char string i) new-string))
    new-string))

(defun break-line-at (string break-bag position)
  "Return the argument STRING with linefeeds inserted at some position past POSITION
   where a character in the break-bag is encountered."
  (let* ((len (length string))
         (new-string (make-array (* 2 len) :element-type 'character :fill-pointer 0)))
    (loop for ix from 0 to (1- (length string))
          with count = 0
          do (vector-push (char string ix) new-string)
          (incf count)
          when (and (> count position)
                    (find (char string ix) break-bag))
          do (vector-push #\Linefeed new-string)
          (setf count 0)
          finally (return new-string))))

(defun read-string-to-list (string)
  (loop with val = nil and start = 0
	do (multiple-value-setq (val start)
	     (read-from-string string nil :eof :start start))
	until (eql val :eof)
	collect val))


;;; (cl-ppcre:split "\\s+" "foo   bar baz frob")
;;; ("foo" "bar" "baz" "frob") 
;;; http://weitz.de/cl-ppcre/#split
(defun split (string c &key min-size)
  "Like the perl split, split the string using the character. Return
   a list of substrings."
  (let ((result
         (loop for i from 0 to (1- (length string))
               with start = 0 with size = 0
	       do (incf size)
               when (and (char= c (char string i))
			 (or (not min-size)
			     (> size min-size)))
               collect (subseq string start i) into result
               and do (setf start (1+ i) size 0)
               finally (return (append result (list (subseq string start)))))))
    (if (zerop (length (first (last result))))
        (butlast result)
      result)))

(defun name2initials (string)
  "For 'abc' return 'a'. For 'product_definition_formation' return 'pdf', etc."
  (let ((result (make-array 31 :element-type 'character :fill-pointer 0))
        (len (length string)))
    (vector-push (char string 0) result)
    (loop for i from 1 to (1- len)
          when (and (char= (char string i) #\_) (< i (1- len))) do
          (vector-push (char string (+ i 1)) result)
          (incf i))
    result))

(defun c-name2lisp (c-string)
  "aNameLikeThis --> a-name-like-this"
  (let* ((len (length c-string))
         (result (make-array (* 2 len) :element-type 'character :fill-pointer 0)))
    (vector-push (char c-string 0) result)
    (loop for i from 1 to (1- len)
          for char = (char c-string i) do		  
          (when (upper-case-p char) (vector-push #\- result))
          (vector-push char result))
    (string-downcase result)))

(defun lisp-name2c (in-string &aux (lisp-string (string-downcase in-string)))
  "a-name-like-this --> aNameLikeThis"
  (let* ((len (length lisp-string))
	 (result (make-array len :element-type 'character :fill-pointer 0)))
    (vector-push (char lisp-string 0) result)
    (loop for i from 1 to (1- len)
	  for char = (char lisp-string i)
          with upper-next = nil do		  
	  (cond ((char= char #\-) 
                 (setf upper-next t))
                (t (vector-push (if upper-next (char-upcase char) char) result)
                   (setf upper-next nil))))
    result))

;;;=============================================
;;; A bunch more from Paul Grahams's "On Lisp."
;;;=============================================
(declaim (inline single-p last1 mklist))

(defun single-p (lst)
  "List contains just one thing."
  (and (consp lst) (not (cdr lst))))

(defun last1 (lst)
  (car (last lst)))

(defun mklist (obj)
  "Make the argument a list if it isn't already."
  (if (listp obj) obj (list obj)))

(defun longer (x y)
  "Return true if x longer than y -- only for lists."
  (labels ((compare (x y)
             (and (consp x)
                  (or (null y)
                      (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
        (compare x y)
      (> (length x) (length y)))))

(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                 (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

(defun prune (test tree)
  " (prune #'oddp '(1 2 (3 4) (5 6 (7 8 (9 10 (11)) 12) 13))) ==> (2 (4) (6 (8 (10 NIL) 12)))"
  (labels ((rec (tree acc)
             (cond ((null tree) (nreverse acc))
                   ((consp (car tree))
                    (rec (cdr tree)
                         (cons (rec (car tree) nil) acc)))
                   (t (rec (cdr tree)
                           (if (funcall test (car tree))
                               acc 
                             (cons (car tree) acc)))))))
    (rec tree nil)))

(defun find2 (fn lst)
  "Like find but returns value from function too."
  (if (null lst)
      nil
    (let ((val (funcall fn (car lst))))
      (if val 
          (values (car lst) val)
        (find2 fn (cdr lst))))))

(defun before (x y lst &key (test #'eql))
  "Returns like member when x before y."
  (and lst
       (let ((first (car lst)))
         (cond ((funcall test y first) nil)
               ((funcall test x first) lst)
               (t (before x y (cdr lst) :test test))))))

(defun after (x y lst &key (test #'eql))
  "Returns like member when x after y. x must be in y."
  (let ((rest (before y x lst :test test)))
    (and rest (member x rest :test test))))

(defun duplicate (obj lst &key (test #'eql))
  "Returns like member when a second copy of obj is in lst.
   > (duplicate 'a '(a b c a d) ==> (A D)"
  (member obj (cdr (member obj lst :test test)) :test test))

(defun split-if (fn lst)
  "Splits a list into two where fn returns true.
   (split-if #'(lambda (x) (> x 4)) '(1 2 3 4 5 6 7) ==> (1 2 3 4) (5 6 7)"
  (let ((acc nil))
    (do ((src lst (cdr src)))
        ((or (null src) (funcall fn (car src)))
         (values (nreverse acc) src))
      (push (car src) acc))))

;;; Why waste your life away typing?
(defmacro mvs (vars form &body body)
  `(multiple-value-setq ,vars ,form ,@body))

(defmacro dbind (vars form &body body)
  `(destructuring-bind ,vars ,form ,@body))

;;; Returns multiple values.
;;; seconds minutes hours days
(defun decode-time-interval (time)
  (multiple-value-bind (days tm1) (floor time 86400) ; (* 60 60 24)
    (multiple-value-bind (hrs tm2) (floor tm1 3600) ; (* 60 60)
      (multiple-value-bind (min sec) (floor tm2 60)
	(values sec min hrs days)))))

(defmacro strcat (&rest strings)
  `(concatenate 'string ,@strings))

(defun now ()
  (multiple-value-bind (s m h d month y) (decode-universal-time (get-universal-time))
    (format nil "~D.~2,'0D.~2,'0D  ~2,'0D:~2,'0D:~2,'0D" y month d h m s)))

;;; Norvig's search routines
(defun tree-search (states goal-p successors combiner &optional do-fn)
  "Find a state that satisfies GOAL-P. Start with STATES, 
   and search according to successors and combiners."
  (cond ((null states) :fail)
        ((funcall goal-p (first states))
	 (when do-fn (funcall do-fn (first states)))
	 (first states))
        (t 
	 (when do-fn (funcall do-fn (first states)))
	 (tree-search
	  (funcall combiner
		   (funcall successors (first states))
		   (rest states))
	  goal-p successors combiner do-fn))))


(let (+search-path+)
  (defun set-search-path (val) (setf +search-path+ val))
  (defun tree-search-path () (mapcar #'car +search-path+))
  (defun backout ()
    (loop while (and +search-path+ (null (first +search-path+))) do
	  (when (and (single-p +search-path+) (null (first +search-path+)))
	    (setf +search-path+ nil)
	    (return-from backout :fail))
	  (pop +search-path+)
	  (setf (first +search-path+) (cdr (first +search-path+)))))
  (defun depth-search-tracking (goal-p successors do)
    "Search depth-first, return path when successful."
    ;;(VARS +search-path+)
    (cond ((eql (backout) :fail) (return-from depth-search-tracking :fail))
	  ((funcall goal-p (caar +search-path+))
	   (when do (funcall do (caar +search-path+)))
	   (mapcar #'car +search-path+))
	  (t 
	   (when do (funcall do (caar +search-path+)))
	   (let ((children (funcall successors (caar +search-path+))))
	     (if children
		 (push children +search-path+)
	       (setf (first +search-path+) (cdr (first +search-path+))))
	     (depth-search-tracking goal-p successors do)))))
)

#|
(defclass node ()
  ((name :reader name :initarg :name)
   (children :reader children :initarg :children)))

(defmethod print-object ((obj node) stream)
  (format stream "[node ~A]" (name obj)))

; a b e f h
(defun tryme (goal)
  (flet ((mn (n &rest c) (make-instance 'node :name n :children c)))
    (let ((a
	   (mn 'a
	       (mn 'b
		   (mn 'd)
		   (mn 'e 
		       (mn 'f 
			   (mn 'h))
		       (mn 'g)))
	       (mn 'c))))
  (depth-first-search a #'(lambda (x) (eql (name x) goal)) #'children 
		      :tracking t
		      :start-state (list (list a))))))
|#

(defun depth-first-search (start goal-p successors &key do tracking start-state)
  "Search new states first until goal is reached."
  (if tracking
      (progn
	(set-search-path (or start-state (list (list start))))
	(depth-search-tracking goal-p successors do))
    (tree-search (or start-state (list start)) goal-p successors #'append do)))

(defun prepend (x y) "Prepend y to start of x" (append y x))

(defun breadth-first-search (start goal-p successors &key do)
  "Search old states first until goal is reached."
  (tree-search (list start) goal-p successors #'prepend do))

(defun fail (arg)
  "Useful in tree searches, when you want to navigate the entire tree."
  (declare (ignore arg))
  nil)

(defmacro update (place object &key (key '#'identity) (test '#'eql))
  "Argument object may have same key as another in the list. If so, replace it with the argument.
   If not, push it. Only replaces first found. (Keys are assumed to be unique)."
  (with-gensyms (obj pos)
    `(let ((,obj ,object))
       (if-bind (,pos (position (funcall ,key ,obj) ,place :key ,key :test ,test))
                (setf ,place (substitute ,obj (nth ,pos ,place) ,place))
                (push ,obj ,place)))))

;;; The body should be a mp:process-run-function.
(defmacro with-stack-size ((size) &body body)
  (declare (ignorable size))
  `(let (#+Lispworks(sys:*sg-default-size* ,size) 
         #-Lispworks())
     ,@body))

;;;   "If in the body you really want double quotes, use escape (e.g. "\"abc\"")
(defmacro pprint-without-strings (&body body)
  `(unwind-protect 
       (progn 
         (set-pprint-dispatch 'string #'(lambda (s x) (let ((*print-pretty* nil)) (format s "~a" x))))
         ,@body)
    (set-pprint-dispatch 'string nil)))

(defmacro pprint-symbols (&body body)
  `(unwind-protect 
       (progn 
         (set-pprint-dispatch 'symbol #'(lambda (s x) (let ((*print-pretty* nil)) (format s "~a" (symbol-name x)))))
         ,@body)
    (set-pprint-dispatch 'symbol nil)))

(defun chop (str)
  "Like perl."
  (if (zerop (length str))
      ""
    (subseq str 0 (1- (length str)))))

(defmacro setx (var val)
  "For use at toplevel to avoid annoying stuff from sbcl and cmucl."
  `(defparameter ,var ,val "A variable set at the top level."))

;;; Sam Steingold
(defun pophash (object ht)
  "Remove the value and return it>"
  (multiple-value-bind (value present-p) (gethash object ht)
    (when present-p (remhash object ht))
    (values value present-p)))

;;; Sam Steingold
(defmacro ensure-gethash (object ht default)
  "Just like GETHASH with the default argument, but DEFAULT is only 
   evaluated when OBJECT is not found and in that case the value of 
   DEFAULT is placed into (GETHASH OBJECT HT)."
  (with-gensyms (obj tab)
   `(let ((,obj ,object) (,tab ,ht))
      (or (gethash ,obj ,tab)
	  (setf (gethash ,obj ,tab) ,default)))))

;;; Sam Steingold
(defmacro map-in (fn seq &rest seqs)
  "`map-into' the first sequence, evaluating it once.
  (map-in F S) == (map-into S F S)"
  (with-gensyms (mi)
    `(let ((,mi ,seq)) (map-into ,mi ,fn ,mi ,@seqs))))

;;; Peter Norvig
(declaim (inline reuse-cons))
(defun reuse-cons (x y x-y)
  "Return (cons x y), or just x-y if it is equal to (cons x y)."
  (if (and (eql x (car x-y)) (eql y (cdr x-y)))
      x-y
    (cons x y)))

;;; SBCL manual, to avoid error when evaluated multiply.
;;; (It is an error if evaluated multiply and the old and new values are not eql).
(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
    ,@(when doc (list doc))))

;;; Resource lists -- An array that is fast in SBCL (:fill-pointer nil :adjustable nil)
;;; that can be stocked with some element and automatically restocked when that element
;;; is depleted. It is also an 'adjustable' array of sorts. 
;;; NOTE: :adjustable-p t arrays are adjustable-array-p = t. This only means that 
;;; adjust-array might return an array identical to the argument. The returned array
;;; need not be adjustable.
(defstruct reslist
  (fillptr 0 :type integer)
  (resize 2.0 :type (or single-float integer))
  (stock-fn nil :type (or null function))
  (arr nil :type simple-vector))

(declaim (ftype (function (t &key (:element-type t) (:resize number) (:stock-fn t)) reslist) new-reslist))
(defun new-reslist (size &key (element-type t) (resize 2.0) stock-fn)
  (let ((reslist (make-reslist
                 :arr (make-array size :element-type element-type)
                 :resize resize :stock-fn stock-fn))) ; pod compile it???
    (when stock-fn
      (let ((arr (reslist-arr reslist)))
        (declare (type simple-vector arr))
        (loop for i from 0 to (1- size) do (setf (aref arr i) (funcall stock-fn))))
      (setf (reslist-fillptr reslist) size))
    reslist))

(declaim (inline reslist-push)
         (ftype (function (t reslist) integer) reslist-push))
(defun reslist-push (val reslist)
  (declare (type reslist reslist))
  (let* ((arr (reslist-arr reslist))
         (size (array-total-size arr))
         (ptr (reslist-fillptr reslist)))
    (declare (type integer ptr size) (type simple-vector arr))
    (when (= ptr size)
      (let ((resize (reslist-resize reslist)))
        (declare (type (or single-float integer) resize))
        (setf (reslist-arr reslist)
                (setf arr 
                        (adjust-array arr 
                           (if (floatp resize) 
                             (floor (* size resize))
                             (+ size resize)))))))
    (setf (aref arr ptr) val)
    (incf (reslist-fillptr reslist))))

(declaim (inline reslist-pop))
(defun reslist-pop (reslist)
  (declare (type reslist reslist))
  (let ((arr (reslist-arr reslist)))
    (declare (type simple-vector arr))
    (when (zerop (reslist-fillptr reslist)) 
      (if-bind (fn (reslist-stock-fn reslist))
       (let ((size (array-total-size arr)))
         (declare (type integer size))
         (loop for i from 0 to (1- size) do
               (setf (aref arr i) (funcall fn)))
         (setf (reslist-fillptr reslist) size))
       (error "Reslist: Nothing left to pop.")))
    (let ((ptr (decf (reslist-fillptr reslist))))
      (declare (type integer ptr))
      (prog1 
        (aref arr ptr)
        (setf (aref arr ptr) nil)))))

(defun intersect-predicates (fn &rest fns)
  "Paul Graham's. Return a predicate which is the AND of the arguments."
  (if (null fns)
      fn
    (let ((chain (apply #'intersect-predicates fns)))
      #'(lambda (x)
          (and (funcall fn x) (funcall chain x))))))


