#|

 Cells Gtk

 Copyright (c) 2004 by Vasilis Margioulas <vasilism@sch.gr>

 You have the right to distribute and use this software as governed by 
 the terms of the Lisp Lesser GNU Public License (LLGPL):

    (http://opensource.franz.com/preamble.html)
 
 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 Lisp Lesser GNU Public License for more details.
 
|#

;;; Todo: separate tree-model/tree-store stuff into another file (used by combo box too).
;;; BTW Tree-store implements the tree-model interface, among other things.

(in-package :cgtk)

(def-object list-store ()
  ((item-types :accessor item-types :initarg :item-types :initform nil)
   (of-tree :accessor of-tree :initform (c-in nil)))
  ()
  ()
  :new-args (c_1 (list (item-types self))))

(def-object tree-store ()
  ((item-types :accessor item-types :initarg :item-types :initform nil)
   (of-tree :accessor of-tree :initform (c-in nil)))
  ()
  ()
  :new-args (c_1 (list (item-types self))))

(defun tv-fail (&rest args) (declare (ignore args)))
(defgeneric get-selection (none))

(def-widget tree-view (container)
  ((columns-def :accessor columns-def :initarg :columns :initform nil)
   (column-types :accessor column-types :initform (c? (mapcar #'first (columns-def self))))
   (column-inits :accessor  column-inits :initform (c? (mapcar #'second (columns-def self))))
   (column-render :accessor column-render 
     :initform (c? (loop for col-def in (columns-def self)
                       for pos from 0 append
                         (when (third col-def)
                           (list pos (third col-def))))))
   (node-render :accessor node-render 
 		:initform (c? (loop for col-def in (columns-def self)
 				 for pos from 0 append
 				 (when (fourth col-def)
 				   (list pos (fourth col-def))))))
   (columns :accessor columns
	    :owning t
     :initform (c? (mapcar #'(lambda (col-init)
                               (apply #'make-be 'tree-view-column
                                      :container self
                                      col-init))
                           (column-inits self))))
   (select-if :unchanged-if #'tv-fail
     :accessor select-if :initarg :select-if :initform (c-in nil))
   (roots :accessor roots :initarg :roots :initform nil)
   (print-fn :accessor print-fn :initarg :print-fn :initform #'identity)
   (children-fn :accessor children-fn :initarg :children-fn :initform #'(lambda (x) (declare (ignore x)) nil))
   (selected-items-cache :cell nil :accessor selected-items-cache :initform nil)
   (selection-mode :accessor selection-mode :initarg :selection-mode :initform :single)
   (expand-all :accessor expand-all :initarg :expand-all :initform (c-in nil))
   (on-select :accessor on-select :initarg :on-select :initform nil)
   (on-edit :accessor on-edit :initarg :on-edit :initform nil)
   (tree-model :owning t :accessor tree-model :initarg :tree-model :initform nil))
  () ; gtk-slots
  () ; signal-slots
  :on-select (lambda (self tree-selection data)
               (declare (ignore widget event data))
	       (with-integrity (:change 'tree-view-select-cb)
		(setf (value self) (get-selection self)))))


(defobserver tree-model ((self tree-view))
  (when new-value
    (gtk-tree-view-set-model (id self) (id new-value))
    (with-integrity (:change 'tv-tree-model)
     (setf (of-tree new-value) self))))

(defobserver expand-all ((self tree-view))
  (when new-value
    (gtk-tree-view-expand-all (id self))))

;;; Used by combo-box also, when it is using a tree model. 
(cffi:defcallback tree-view-items-selector :void
  ((model :pointer) (path :pointer) (iter :pointer) (data :pointer))
  (declare (ignore path data))
  (let ((tree (of-tree (gtk-object-find model))))
    (push (item-from-path (children-fn tree)
            (roots tree)
			  (read-from-string 
			   (gtk-tree-model-get-cell model iter (length (column-types tree)) :string)))
          (selected-items-cache tree)))
  0)

(defmethod get-selection ((self tree-view))
  (let ((selection (gtk-tree-view-get-selection (id self)))
        (cb (cffi:get-callback 'tree-view-items-selector)))
    (setf (selected-items-cache self) nil)
    (gtk-tree-selection-selected-foreach selection cb +c-null+)
    (if (equal (gtk-tree-selection-get-mode selection) 3) ;;multiple
      (copy-list (selected-items-cache self))
    (first (selected-items-cache self)))))


(defobserver selection-mode ((self tree-view))
  (when new-value
    (let ((sel (gtk-tree-view-get-selection (id self))))
      (gtk-tree-selection-set-mode sel 
	 (ecase (selection-mode self)
	   (:none 0)
	   (:single 1)
	   (:browse 2)
	   (:multiple 3))))))

(cffi:defcallback tree-view-select-handler :void
    ((tree-selection :pointer) (data :pointer))
  (if-bind (tree-view (gtk-object-find tree-selection))
           (let ((cb (callback-recover tree-view :on-select)))
             (funcall cb tree-view tree-selection data))
           (trc "Clean up old widgets after runs" tree-selection))
  0)

;;; The check that previously was performed here (for a clos object) caused the handler
;;; not to be registered (a problem of execution ordering?). Anyway, do we need such a check?
(defobserver on-select ((self tree-view))
  (when  new-value    
    (let ((selected-widget (gtk-tree-view-get-selection (id self))))
      (gtk-object-store selected-widget self) ;; tie column widget to clos tree-view
      (callback-register self :on-select new-value)
      (let ((cb (cffi:get-callback 'tree-view-select-handler)))
        ;(trc nil "tree-view on-select pcb:" cb selected-widget "changed")
        (gtk-signal-connect selected-widget "changed" cb)))))

;;;
;;; Listbox submodel
;;; 

(defmodel listbox (tree-view)
  ((roots :initarg :items)) ; alternate initarg for inherited slot
  (:default-initargs 
      :tree-model (c? (make-instance 'list-store
				:item-types (append (column-types self) (list :string))))))

(defmethod items ((self listbox))
  (roots self))

(defmethod (setf items) (val (self listbox))
  (setf (roots self) val))

(defun mk-listbox (&rest inits)
  (assert *parent*)
  (let ((self (apply 'make-instance 'listbox (append inits (list :fm-parent *parent*)))))
    (with-integrity (:change 'mk-listbox-of-tree)
      (setf (of-tree (tree-model self)) self))
    self))

(defobserver select-if ((self listbox))
  (when new-value
    (with-integrity (:change 'listbox-select-if-observer)
      (setf (value self) (remove-if-not new-value (roots self))))))

(defobserver roots ((self listbox))
  (when old-value
    (gtk-list-store-clear (id (tree-model self))))
  (when new-value
    (gtk-list-store-set-items 
     (id (tree-model self)) 
     (append (column-types self) (list :string))
     (loop for item in new-value
	  for index from 0
         collect (let ((i (funcall (print-fn self) item)))
                   ;(ukt:trc nil "items output: old,new" item i)
                   (append i
                     (list (format nil "(~d)" index))))))))

;;;
;;; Treebox submodel
;;;

(defmodel treebox (tree-view)
  ()
  (:default-initargs 
      :tree-model (c? (mk-tree-store
                        :item-types (append (column-types self) (list :string))))))

(defun mk-treebox (&rest inits)
  (assert *parent*)
  (let ((self (apply 'make-instance 'treebox (append inits (list :fm-parent *parent*)))))
    (with-integrity (:change 'mk-treebox-of-tree)
      (setf (of-tree (tree-model self)) self))
    self))

(defobserver select-if ((self treebox))
  (when new-value
     (with-integrity (:change 'treebox-obs-select-if)
       (setf (value self) (mapcan (lambda (item) (fm-collect-if item new-value)) 
                            (roots self))))))

(defobserver roots ((self treebox))
  (when old-value
    (gtk-tree-store-clear (id (tree-model self))))
  (when new-value
    (loop for root in new-value
       for index from 0 do
	 (gtk-tree-store-set-kids (id (tree-model self)) root +c-null+ index
				  (append (column-types self) (list :string)) 
				  (print-fn self) (children-fn self)))
    (when (expand-all self)
      (gtk-tree-view-expand-all (id self)))))

;;; These look like ("Trimmed Text" "(0 0 )") for example where menu structure is "Text --> Trimmed Text"
;;; Column-types is a list of :string, :float etc. used to reference g-value-set-string etc.
(defun gtk-tree-store-set-kids (model val-tree parent-iter index column-types print-fn children-fn &optional path)
  (with-tree-iter (iter)
    (gtk-tree-store-append model iter parent-iter) ; sets iter
    (gtk-tree-store-set model iter	; Not a gtk function!
			column-types
			(append
			 (funcall print-fn val-tree)
			 (list (format nil "(~{~d ~})" (reverse (cons index path))))))
    (loop for sub-tree in (funcall children-fn val-tree)
       for pos from 0 do
       (gtk-tree-store-set-kids model sub-tree iter
				pos column-types print-fn children-fn (cons index path)))))



;;;
;;; Cell rendering
;;; 

(cffi:defcallback tree-view-render-cell-callback :void
  ((tree-column :pointer) (cell-renderer :pointer) (tree-model :pointer) 
   (iter :pointer) (data :pointer))
  (if-bind (self (gtk-object-find tree-column))
       (let ((cb (callback-recover self :render-cell)))
         (assert cb nil "no :render-cell callback for ~a" self)
         (funcall cb tree-column cell-renderer tree-model iter data))
       (trc nil "Clean up old widgets from prior runs." tree-column))
  1)

(defun item-from-path (child-fn roots path)
  (loop for index in path
        for node = (nth index roots) then (nth index (if node (funcall child-fn node) (return nil)))
        finally (return node)))

(declaim (optimize (debug 3)))

(defun gtk-tree-view-render-cell (col col-type cell-attrib-f &optional node-attrib-f) 
  (trc nil "gtv-render-cell> creating callback" col col-type cell-attrib-f)
  (flet ((node-from-iter (model iter)
	   (when-bind* ((tree-model (gtk-object-find model))
			(tree-view (of-tree tree-model))
			(path (gtk-tree-model-get-cell model iter (length (column-types tree-view)) :string)))
	     (item-from-path (children-fn tree-view) 
			     (roots tree-view)
			     (read-from-string path)))))
    (lambda (tree-column cell-renderer model iter data)
      (DECLARE (ignorable tree-column data))
      (trc nil "gtv-render-cell (callback)> entry"
	       tree-column cell-renderer model iter data)
      (let ((item-value (gtk-tree-model-get-typed-item-value model
							     iter
							     col
							     col-type))
	    (node (node-from-iter model iter)))	
	(trc nil "gtv-render-cell (callback)> rendering value"
		 col col-type ret$ item-value)
       
	(apply #'gtk-object-set-property cell-renderer 
	       (case col-type 
		 (:boolean (list "active" 'gboolean item-value))
		 (:icon (list "stock-id" 'gtk-string
			      (string-downcase (format nil "gtk-~a" item-value))))
		 (t (list "text" 'gtk-string
			  (case col-type
			    (:date (multiple-value-bind (sec min hour day month year) 
				       (decode-universal-time (truncate item-value))
				     (format nil "~2,'0D/~2,'0D/~D ~2,'0D:~2,'0D:~2,'0D" 
					     day month year hour min sec)))
			    (:string (if item-value (get-gtk-string item-value) ""))
			    (otherwise (format nil "~a" item-value)))))))

      
	(when cell-attrib-f
	  (gtk-cell-renderer-set-attribs cell-renderer (funcall cell-attrib-f item-value)))
	(when (and node node-attrib-f)
	  (gtk-cell-renderer-set-attribs cell-renderer (funcall node-attrib-f node))))
      1)))

;;;
;;; Editable cells
;;;

(defstruct renderer
  tree-view col)

;;; a hash table to keep track of the renderer objects

(let ((renderers (make-hash-table)))
  (defun register-renderer-data (renderer data)
    (setf (gethash (cffi-sys:pointer-address renderer) renderers) data))
  (defun recover-renderer-data (renderer)
    (gethash (cffi-sys:pointer-address renderer) renderers)))

;;; generic callback -- update treestore and call on-edit func

(defun gtk-path-to-list (path)
  "converts \"1:2\" to (1 2)"
  (read-from-string (format nil "(~a)" (map 'string #'(lambda (c) (if (eql c #\:) #\space c)) path))))


(defun tree-view-edit-cell-callback (renderer path new-value)
  (if-bind (data (recover-renderer-data renderer))
	   (let* ((tree (renderer-tree-view data))
		  (model (id (tree-model tree)))
		  (col (renderer-col data))
		  (col-type (nth col (column-types tree)))
		  (fn (on-edit tree))
		  (path (cffi:foreign-string-to-lisp path))
		  (node (item-from-path #'kids (roots tree) (gtk-path-to-list path))))
	     #+msg (format t "~&Edited path ~a --> node ~a~%" (gtk-path-to-list path) (when node (md-name node)))
	     (when node 
	      (with-tree-iter (iter)
		(gtk-tree-model-get-iter-from-string (id (tree-model tree)) iter path)
		(let ((new-val (case col-type
				 (:boolean (not (gtk-tree-model-get-cell model iter col :boolean))) ; toggle boolean cell,
				 (t new-value))))
		  #+msg (format t "~&Setting value for ~a to ~a ..." node new-val)
		  (gtk-tree-store-set-cell model iter col col-type new-val)
		  (funcall fn node col new-val))) ; call setf function
	      #+msg (format t " done.~%")
	      (force-output)))
	   (warn (format nil "No callback registered "))))

;;; a tribute to static typing

(cffi:defcallback tree-view-edit-cell-callback-string :void
    ((renderer :pointer) (path :pointer) (new-value gtk-string) (user-data :pointer))
  (declare (ignore :pointer))
  (tree-view-edit-cell-callback renderer path new-value)
  1)

(cffi:defcallback tree-view-edit-cell-callback-boolean :void
    ((renderer :pointer) (path :pointer) (data :pointer))
  (declare (ignore data))
  (tree-view-edit-cell-callback renderer path nil)
  1)


;;;
;;; echo functions for tree-view
;;;

(defobserver columns ((self tree-view))
  (when old-value 
    (loop for col in old-value do
        (gtk-tree-view-remove-column (id self) (id col))
        #+not-necessary (gtk-object-forget (id col) col)))  ; ph 042008
  (when new-value
    (loop for col in new-value
	for pos from 0
	for item-renderer = (fourth (nth pos (columns-def self)))
	 for col-type = (nth pos (column-types self))
        for renderer = (case col-type
                         (:boolean (gtk-cell-renderer-toggle-new))
                         (:icon (gtk-cell-renderer-pixbuf-new))
                         (t (gtk-cell-renderer-text-new))) do
          (gtk-tree-view-column-pack-start (id col) renderer t)
          (gtk-tree-view-column-set-cell-data-func (id col) renderer
            (let ((cb (cffi:get-callback 'tree-view-render-cell-callback)))
              ;(trc nil "tree-view columns pcb:" cb (id col) :render-cell)
              (callback-register col :render-cell
                (gtk-tree-view-render-cell pos 
                  (nth pos (column-types self))
                  (getf (column-render self) pos)
		  (getf (node-render self) pos)))
              cb)
            +c-null+ +c-null+)
	 ;; register renderer for edit callback
	 (when (on-edit self)
	   (register-renderer-data renderer
				   (make-renderer :tree-view self
						 :col pos))
	   (case col-type
	     (:string (gtk-signal-connect renderer "edited" (cffi:get-callback 'tree-view-edit-cell-callback-string)))
	     (:boolean (gtk-signal-connect renderer "toggled" (cffi:get-callback 'tree-view-edit-cell-callback-boolean)))))
	 (gtk-tree-view-column-set-sort-column-id (id col) (or (sort-column-id col)
                                                               pos))
	 (gtk-tree-view-append-column (id self) (id col)))))

(def-object tree-view-column ()
  ((title :accessor title :initarg :title :initform nil)
   (visible :accessor visible :initarg :visible :initform t))
  (spacing resizable fixed-width min-width max-width expand clickable
   sort-column-id sort-indicator reorderable)
  ()
  :resizable t
  :expand t
  :reorderable t)

(defobserver visible ((self tree-view-column))
  (gtk-tree-view-column-set-visible (id self) new-value))

(defobserver title ((self tree-view-column))
  (when new-value
    (gtk-tree-view-column-set-title (id self) new-value)))


(defmacro def-columns (&body args)
  "Convencience macro for defining tree-view columns.  args has the form col-def*, 
where col-def ::= (type inits renderer item-renderer).

type     -- is :boolean, :icon, or :text (default)
inits    -- '(:title \"name\")
renderer -- a fn of the cell value returning a plist.  Allowed attribs are
:font, :size, :strikethrough, :foreground, :background, :editable
node-renderer -- the same, but a function of a node, not the print-fn value of it."
  `(list ,@(loop for (type inits renderer node-renderer) in args collect
		 `(list ,type ',inits ,renderer ,node-renderer))))

;;;
;;; Tree observer tree view
;;;

;;; this is an alternative mode of operating a tree view -- instead of have it traverse roots once
;;; and build the corresponding tree model, we create a family of observers that maps every node to
;;; the corresponding row in the tree view, yielding a live update


;;;
;;; Debugging tool
;;;

(eval-when (:compile-toplevel :load-toplevel)
  (defparameter *with-debug* t))

(defmacro with-trc (form)
    (if *with-debug*
	(with-gensyms (res)
	  `(progn
	     (format t "~&eval ~a " ',form)
	     (let ((,res ,form))
	       (format t "--> ~a~%" ,res)
	       (force-output)
	       ,res)))
	form))

(defmacro with-trcs (&body body)
  (if *with-debug*
      `(progn
	 ,@(mapcar #'(lambda (f) `(with-trc ,f)) body))
      `(progn ,@body)))


;;; first the cells stuff -- base class

(defun deadp (cell)
  (eql (slot-value cell 'cells::.md-state) :eternal-rest))


(defmethod mk-observer ((parent t) (source family) &rest initargs)
  "Used internally by family-observer and its sublasses to create kids.  Use (make-be 'your-observer :value source) to create a family observer.

Creates an observer node observing source.  To be specialized on subclasses of family observer and source"
  (apply #'make-instance 'family-observer :value source :fm-parent *parent* initargs))

(defmodel family-observer (family)
  ;; we'll use the "value" slot for the observed
  ()
  (:default-initargs
      :kids (kids-list?
	     (progn #+msg(print (list "CALCULATE KIDS for family observer" self "on" (^value) "-- parent" (upper self)))
		    (bwhen (val (^value)) ;; not sure why not
		      (unless (deadp val)
			(trcx nil "creating kids" val (slot-value val 'cells::.md-state) (kids val))
			(mapcar #'(lambda (src) (mk-observer self src)) (kids val))))))))

;;; here do cleanup work, children get called before parents
(defmethod not-to-be :before ((self family-observer))
  #+msg (print (list "DESTROY family observer" self "on" (value self))))

;;; this is too early -- upper self is not set yet
(defmethod initialize-instance :after ((self family-observer) &rest initargs)
  (declare (ignorable initargs))
  #+msg (print (list "CREATE family observer" self  "on" (value self) "-- parent" (upper self))))

;;; this is too late, gets called for children before parent
(defmethod md-awaken :after ((self family-observer))
  #+msg (print (list "AWAKEN family observer" self "on" (value self) "-- parent" (upper self))))


;;; then the cells stuff for observing slots

(defmacro def-f-observer (name (&rest superclasses) (&optional (source 'family)) &body slots)
  "create an observer derived from superclasses observing slots of objects of type source"
  (multiple-value-bind (slot-defs output-fns)
      (loop for slot in slots
	 for slot-def = (if (atom slot) (list slot) slot)
	 for slot-name = (car slot-def)
	 for slot-plist = (cdr slot-def)
	 for reader-fn = (getf slot-plist :reader-fn `#'(lambda (value) (identity value)))
	 for output-fn = (getf slot-plist :output-fn nil)
	 collecting `(,slot-name :reader ,slot-name :initform (c? (bwhen (val (^value)) (funcall ,reader-fn (,slot-name val))))) into slot-defs
	 if output-fn collecting `(defobserver ,slot-name ((self ,name))
				    (with-integrity (:change ',slot-name)
				     (funcall ,output-fn self new-value))) into output-fns
	 finally (return (values slot-defs output-fns)))
    `(progn
       (eval-now!
	 (defmodel ,name ,superclasses
	   ,slot-defs)
	 ,@output-fns)
       (defmethod mk-observer ((parent ,name) (source ,source) &rest initargs)
	 (apply #'make-instance ',name :value source :fm-parent *parent* initargs)))))
;;; 
;;; GTK tree row infrastructure
;;; 

(defun tree-row-valid (&rest row-references)
  (every #'(lambda (row) (and row (gtk-tree-row-reference-valid row))) row-references))

(defmacro with-tree-row ((row &key (default-model nil default-model-p)) &body body)
  "Executes body with iter and model/path pointing to row.  When row is a null-pointer, model is bound to default-model -- 
without default-pointer, body is not executed -- path and iter are null-pointer.  When row is nil, the body is not executed."
  `(when ,row 
     (if (not (cffi-sys:null-pointer-p ,row))
	 (if (tree-row-valid ,row)
	     (with-tree-iter (iter)
	       (let ((model (gtk-tree-row-reference-get-model ,row))
		     (path (gtk-tree-row-reference-get-path ,row)))
		 (gtk-tree-model-get-iter model iter path)
		 (unwind-protect
		      (progn ,@body)
		   (gtk-tree-path-free path))))
	     (format t "~&WARNING: with-tree-row called with invalid row-reference.  body skipped~%"))
	 ,(when default-model-p
		`(if ,default-model
		     (let ((model ,default-model) (path +c-null+) (iter +c-null+))
		       (declare (ignorable model path iter))
		       ,@body)
		     (format t "~&WARNING: with-tree-row called with null-pointer and no default model given.  body is skipped~%"))))))

(defun tree-row-create (parent-row-reference parent-gtk-id)
  "creates a gtk row below parent and returns a row reference pointing to it."
  (with-tree-row (parent-row-reference :default-model parent-gtk-id)
    (with-tree-iter (i-new)
      (gtk-tree-store-append model i-new iter)
      #+msg (format t " row created ")
      (gtk-tree-row-reference-new model (gtk-tree-model-get-path model i-new)))))

(defun tree-row-set-cell (row-reference col data)
  (with-tree-row (row-reference)
    #+msg (format t " set cell ")
    (unless (gtk-object-find model) (break))
    (gtk-tree-store-set-cell model iter col (nth col (item-types (gtk-object-find model))) data)))

(defun tree-row-get-cell (row-reference col)
  (with-tree-row (row-reference)
    #+msg (format t " get cell ")
    (gtk-tree-model-get-cell model iter col (nth col (item-types (gtk-object-find model))))))

(defun tree-row-set-path (row-reference parent-row-reference position)
  "sets the path information by concatenating the path of parent-row and position"
  (when (tree-row-valid row-reference)
    #+msg (format t "~&setting path -- ")
    (let* ((col (1- (length (item-types (gtk-object-find (gtk-tree-row-reference-get-model row-reference))))))
	   (path (write-to-string (concatenate 'list
					       (when (tree-row-valid parent-row-reference)
						 (read-from-string (tree-row-get-cell parent-row-reference col)))
					       (list position)))))
      (tree-row-set-cell row-reference col path)
      #+msg (format t " -- done.~%")
      path)))

(defun tree-row-destroy (row-reference)
  "deletes the tree row referenced by row-reference and frees the reference"
  (when (tree-row-valid row-reference)
    (with-tree-row (row-reference)
      (gtk-tree-store-remove model iter))
    (gtk-tree-row-reference-free row-reference)
    #+msg (format t " row deleted ")))


;;; 
;;; the CELLS-TREE-NODE is a node mapping a CLOS object to a GTK tree row
;;;

(eval-when (:load-toplevel :execute)
  (defmodel cells-tree-node (family-observer)
    ((row :reader row :initarg :row))
    (:default-initargs
	:row (c? (when-bind* ((parent (upper self)) (pos (position self (kids parent))))
		   (unless (or (deadp parent) (deadp self))
		     #+msg (format t "~&create row for ~a (parent ~a) -- " (value self) (value parent)) 
		     (let ((new-row (tree-row-create (row parent) (id parent))))
		       (when (tree-row-valid new-row)
			 #+msg (format t " -- row for ~a is valid~%" (value self)) 
			 (tree-row-set-path new-row (row parent) pos)
			 new-row))))))))

;;; gtk-object-forget is called recursively on all the children of tree-store
(defmethod id ((self cells-tree-node))
  (cffi-sys:null-pointer))

;;; if the row changes, remove the old one
(defobserver row ((self cells-tree-node))
  (unless (deadp self)
   (when old-value
     #+msg (format t "~&destroying OLD row for ~a -- " (value self))
     (tree-row-destroy old-value)
     #+msg (trc "-- done"))))

;;; not-to-be --> remove the row (:around runs on the way back up, thus deleting children before parents)
(defmethod not-to-be :before ((self cells-tree-node))
  (unless (deadp self)
    #+msg (format t "~&destroying row for ~a -- " (value self))
    (tree-row-destroy (row self))
    #+msg (trc "-- done")))

(defmacro def-cells-tree-node (name (&key (superclass 'cells-tree-node) (source-type 'family)) &body slots)
  "create a cells-tree-node class with columns matching slots.
For each slot you can specify :type, :reader-fn (value), :output-fn, :title, :render-fn"
  (let ((slot-lists (mapcar #'(lambda (slot) (if (atom slot) (list slot) slot)) slots)))
    `(def-f-observer ,name (,superclass) (,source-type)
       ,@(loop for slot in slot-lists
	    for col from 0
	    for slot-name = (car slot)
	    collecting (destructuring-bind
			     (&key (type :string)
				   (reader-fn (case type
						(:string `#'(lambda (value) (prin1-to-string value)))
						(t `#'identity)))
				   output-fn
				   title
				   render-fn &allow-other-keys) (cdr slot)
			 (declare (ignorable title render-fn))
			 `(,slot-name
			   :reader-fn ,reader-fn
			   :output-fn #'(lambda (self new-value)
					  (unless (deadp self)
					   ,(when output-fn `(funcall ,output-fn self new-value))
					    (when (tree-row-valid (row self))
					      #+msg (format t ,(format nil "~~&updating slot ~a for ~~a " slot-name) (value self))
					      (tree-row-set-cell (row self) ,col new-value)
					      #+msg (format t "done~%"))))))))))

;;; 
;;; the CELLS-TREE-STORE is a tree-store that is also a map for the source node
;;;

(defmodel cells-tree-store (family-observer tree-store)
  ((row :reader row :initform +c-null+)))


;;; 
;;; the CELLS-TREE-VIEW widget
;;;

(defmodel cells-tree-view (tree-view)
  ((source :initform (c-in nil) :initarg :source :accessor source)
   (tree-model :accessor tree-model :owning t :initarg :tree-model :initform (c-in nil))
   (tree-model-type :initarg :tree-model-type :initform (error "supply a tree-model-type for cells-tree-view upon instantiation") :accessor tree-model-type)))

(defobserver source ((self cells-tree-view))
  (with-integrity (:change 'make-tree-model)
   (setf (tree-model self) (make-instance (tree-model-type self) :value new-value))))

(defmethod roots ((self cells-tree-view))
  (and (tree-model self) (value (tree-model self)) (kids (value (tree-model self)))))


(defmacro def-cells-tree-view (name (&key (super-node 'cells-tree-node) (source-type 'family)) &body slots)
  "creates a cells-tree-view widget using cells-tree-nodes as if defined with def-cells-tree-node.  The widget can be instantiated with mk-`name`"
  (let ((node (intern (format nil "~a-NODE" name)))
	(store (intern (format nil "~a-STORE" name)))
	(on-edit-fn (intern (format nil "~a-ON-EDIT" name)))
	(constructor (intern (format nil "MK-~a" name)))
	(slots (mapcar #'(lambda (slot) (if (atom slot) (list slot) slot)) slots)))
    `(progn
       (def-cells-tree-node ,node (:superclass ,super-node :source-type ,source-type)
	 ,@slots)
       (defmodel ,store (cells-tree-store)
	 ()
	 (:default-initargs
	     :item-types ',(append
			    (mapcar #'(lambda (slot) (getf (cdr slot) :type :string)) slots)
			    (list :string))))
       (defmethod mk-observer ((parent ,store) (source ,source-type) &rest initargs)
	 (apply #'make-instance ',node :value source :fm-parent *parent* initargs))
       ,@(loop
	    for slot in slots
	    for col from 0
	    for writer-fn = (getf (cdr slot) :writer-fn `#'identity)
	    collect `(defmethod ,on-edit-fn (node (col (eql ,col)) new-value) ; node is part of the source tree!
		       #+msg (format t ,(format nil "~~&setf'ing ~a on ~~a -- " (car slot)) node)
		       (with-integrity (:change 'setf-data)
			(setf (,(car slot) node) (funcall ,writer-fn new-value)))
		       #+msg (format t " -- done.~%")))
       (defun ,constructor (source &rest inits)
	 (apply #'make-instance 'cells-tree-view
		:fm-parent *parent*
		:source (c-in source)       
		:children-fn #'kids
		:on-edit #',on-edit-fn
		:tree-model-type ',store
		:roots (c-in nil)
		:columns (list ,@(mapcar #'(lambda (slot)
					     (destructuring-bind (&key (type :string)
								       (title (format nil "~:(~a~)" (car slot))) 
								       render-fn &allow-other-keys)
						 (cdr slot)
					       `(list ,type (list :title ,title) nil ,render-fn))) slots))
		inits)))))

;;;
;;; The object inspector
;;;

#|

;;; under development

(defmodel slot-inspector (cells-tree-node)
  ()
  (:default-initargs :kids (c-in nil)))

(defobserver md-name ((self slot-inspector))
 (when (tree-row-valid (row self))
   (tree-row-set-cell (row self) 'col new-value)))

(defobserver value ((self slot-inspector))
 (when (tree-row-valid (row self))
   (tree-row-set-cell (row self) 'col new-value)))



(defmodel obj-inspector (cells-tree-node)
  ()
  (:default-initargs :kids (c-in nil)))

(defmacro def-obj-inspector (object-type &body slots)
  `(defmethod mk-obj-inspector (parent (obj ,object-type))
     (push (make-instance 'obj-inspector
			  :kids (the-kids ,(loop for raw-slot in slots
				     for slot = (if (listp raw-slot) raw-slot (list raw-slot))
				     collecting (destructuring-bind (slot-name &key (title ))`(make-instance 'slot-inspector
													     :md-name 'sth
													     :value (c? ))))))
      (kids parent))))

(defmodel cells-inspector-store (tree-store)
  ((row :reader row :initform +c-null+))
  (:default-initargs :kids (c-in nil)))


(defmodel cells-inspector (cells-tree-view)
  ()
  )

(defmethod mk-obj-inspector ((object family) (parent cells-inspector)))

(defmacro def-inspector-map (object &body slots)
  )

|#