#|
 Copyright (c) 2005 by Peter Denno <peter.denno@nist.gov>

 You have the right to distribute and use this software as governed by 
 the terms of the Lisp Lesser GNU Public License (LLGPL):

    (http://opensource.franz.com/preamble.html)
 
 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 Lisp Lesser GNU Public License for more details.
 
|#


(in-package :cgtk)

;;;; ==========================================================================
;;;;     debug facility
;;;; ==========================================================================

(defparameter *debug* t)

(defmacro deb (format-string &rest params)
  `(when *debug*
     (format t ,(concatenate 'string "~&" format-string "~%") ,@params)
     (force-output)))


;;;; ==========================================================================
;;;;     convenience macros
;;;; ==========================================================================



(defmacro defmodel2 (name (&rest superclasses) (&rest slots) &rest definitions)
  "Defines a model like defmodel, with a few enhancements: Slots are by default provided with the corresponding 
accessors and initargs.   If you provide more than two atoms, your slot definition is untouched.
The optional definitions understand :kids-entries,
:readers, :from-upper, :initialize-instance.  :default-initargs is understood, buts its value 
neesds to be wrapped in parens."
  (destructuring-bind
	(&key kids-entries readers from-upper initialize-instance default-initargs) (car definitions)                  
    (let ((slots (loop for slot in slots
		    for slot-name = (if (atom slot) slot (car slot))
		    for initform = (if (atom slot) nil (second slot))
		    if (cddr slot) collecting slot
		    else collecting `(,slot-name :initform (c-in ,initform) 
						 :initarg ,(intern (string slot-name) :keyword)
						 :accessor ,slot-name)))     
	  (reader-slots (loop for slot in readers
			   for slot-name = (car slot)
			   for initform = (second slot)
			   collecting `(,slot-name :initform (c? ,initform)
						   :reader ,slot-name)))
	  (from-upper-slots (loop for slot-name in from-upper
			       collecting `(,slot-name :initform (c? (,slot-name (upper self)))
						       :reader ,slot-name))))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
	 (defmodel ,name (,@superclasses)
	   (,@slots
	    ,@reader-slots
	    ,@from-upper-slots)
	   ,(when default-initargs
		  `(:default-initargs ,@default-initargs)))
	 (eval-when (:load-toplevel :execute)
	   ,(when (or kids-entries initialize-instance)
		  (with-gensyms (rest)
		    `(defmethod initialize-instance ((self ,name) &rest ,rest)
		       (declare (ignorable ,rest))
		       (call-next-method-when)
		       ,@(when kids-entries
			       (loop for kid in kids-entries
				  collecting `(push ,(append kid '(:fm-parent self)) (kids self))))
		       ,@initialize-instance))))))))


(defmacro call-next-method-when ()
  "call-next-method if and only if there is one"
  `(when (next-method-p)
     (call-next-method))) 

(defmacro funcall-when (fn &rest params)
  "funcalls fn if and only if fn is non-nil"
  `(when ,fn
     (funcall ,fn ,@params)))

(defun true (val)
  (if val t))

(defmacro with-slot-accessors ((&rest slots) obj &body body)
  "like with-slots, but using accessors, thus cell-safe"
  `(with-accessors ,(loop for slot in slots collecting (list slot slot)) ,obj ,@body))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun strip-properties (plst keys)
    "conses a fresh plist without the members of plist and their values"
    (labels ((strip (lst) (when (consp lst)
			    (if (member (car lst) keys) (strip (cddr lst))
				(cons (car lst) (cons (cadr lst) (strip (cddr lst))))))))
      (strip plst))))


;;;; ==========================================================================
;;;; the drawing area widget
;;;; ==========================================================================


(defmodel cairo-drawing-area (drawing-area)
  ((cairo-context :accessor cairo-context :cell nil :initform nil)
   (canvas        :accessor canvas        :initform (c-in nil) :initarg :canvas :owning t)
   (.canvas :accessor .canvas :initform (c-in nil) :owning t)
   (prims :reader prims :initform (c? (append (canvas self) (.canvas self))))
   (widget        :reader widget          :initform (c? self))
   ;; the primitive the mouse is currently hovering over
   (hover         :accessor hover         :cell nil :initform nil)
   (hover-history :accessor hover-history :cell nil :initform nil)
   ;; callback (on-clicked [widget] [button] [primitive] [pos])
   (on-clicked    :accessor on-clicked    :cell nil :initform nil :initarg :on-clicked)
   ;; callback (on-dragged [widget] [button] [primtitive] [start] [end])
   (on-dragged    :accessor on-dragged    :cell nil :initform nil :initarg :on-dragged)
   
   (dragging      :accessor dragging      :initform (c-in nil))
   
   (drag-start    :accessor drag-start    :cell nil :initform nil)
   (drag-offset   :accessor drag-offset   :cell nil :initform nil)
   (button-down-position :accessor button-down-position :cell nil :initform nil)
   (select-box    :accessor select-box    :cell nil :initform nil)
   
   (selection-color :accessor selection-color :cell nil :initform '(1 1 .27))
   (drag-threshold :accessor drag-threshold :cell nil :initform 3 :initarg :drag-threshold)
   (selection     :accessor selection     :initform (c-in nil)))
   (:default-initargs
       :on-pressed #'cairo-drawing-area-button-press
     :on-released #'cairo-drawing-area-button-release
     :on-moved #'cairo-drawing-area-motion
     :on-draw #'cairo-drawing-area-draw))


;;;; ==========================================================================
;;;; graphic elements
;;;; ==========================================================================

;;; creates a primitve for use in a drawing area widget
  

(defmacro defprimitive (name (&rest superclasses) (&rest slots) &rest definitions)
  "Defines a graphic primitive based on SUPERCLASS with the given SLOTS.  Accessors are
created automatically.  Furthermore listeners are created that toggle a REDRAW event
when slot values are SETF'd."
  (destructuring-bind
   (&key no-redraw readers from-upper &allow-other-keys) (car definitions)
   (let ((outputs (loop for slot in (append slots readers from-upper)
                        for slot-name = (if (atom slot) slot (car slot))      
                        collecting (unless (member slot-name no-redraw)
                                     `(defobserver ,slot-name ((self ,name) new-val)
                                        (when-bind (widget (widget self))
                                          (redraw widget))))))
         (other-defs (strip-properties (car definitions) '(:no-redraw))))
     `(progn
        (defmodel2 ,name (,@(or superclasses '(primitive)))
          ,slots
          ,other-defs)
       ,@outputs))))



;;;; -----------------------------------------------------------
;;;;        drawing method 
;;;; -----------------------------------------------------------


(defmacro defdraw (class-name (&rest slots) &body body)
  "Defines a draw method on class CLASS-NAME.  In the BODY, the SLOTS will be bound
to the corresponding slot accessors of CLASS-NAME.  Furthermore CONTEXT will be bound to 
a current cairo context and KIDS to the kids of CLASS-NAME.  The method will automatically
call NEXT-METHOD and finally map itself over all kids."
  `(defmethod draw ((self ,class-name))
     (when (widget self)
       (with-accessors
	     (,@(loop for slot in slots collecting `(,slot ,slot))
	      (kids kids))
	   self
         (cl-cairo2:with-context ((cairo-context (widget self)))
           ,@body
           (call-next-method-when)
	   (trc nil "drawing kids?" kids (listp kids))
	   (when (listp kids)
	     (mapcar #'draw kids)))))))
  
(defgeneric draw (element)
  (:documentation "draws a primitive in the context stored in the cairo-context slot of the associated widget"))

;; a handler if draw called on nil
(defmethod draw (self))


;;;; -----------------------------------------------------------
;;;;        export method 
;;;; -----------------------------------------------------------

(defmethod export-to-file ((self cairo-drawing-area) file-name &key (type :ps) (width 500) (height 500))
  (deb "Exporting to ~a" file-name)
  (with-slots (cairo-context prims) self
              (setf cairo-context
                    (funcall (case type
                                   (:ps #'cl-cairo2:create-ps-context)
                                   (:pdf #'cl-cairo2:create-pdf-context))
                             file-name
                             width height))
              (mapcar #'draw prims)
              (cl-cairo2:destroy cairo-context))
  (deb "done."))


(defobserver prims ((self cairo-drawing-area))
  (redraw self))


;;;; -----------------------------------------------------------
;;;;     modify method (to change several parameters at once) 
;;;; -----------------------------------------------------------

;;;; ---- convenience macros for modify method -----------------

(defmacro when-supplied-setf (place property-list property)
  "if PROPERTY is supplied in the PROPERTY-LIST, the corresponding value is setf'd to PLACE"
  (let ((value-sym (gensym)))
    `(let ((,value-sym (getf ,property-list ,property)))
       (when ,value-sym
         (setf ,place ,value-sym)))))

(defmacro property-list-setf ((object property-list) &rest properties)
  "Calls accessors on OBJECT with the corresponding values given in PROPERTY-LIST.  PROPERTIES is of
the form (<accessor> <:key>)*, where KEY is the property for PROPERTY-LIST and ACCESSOR the corresponding
accessor on OBJECT.  This is used to conveniently build update functions."
  (let ((cmds (loop for feature in properties
                    collecting (list 'when-supplied-setf (list (car feature) object) property-list (cadr feature)))))
    `(progn ,@cmds)))


;;;; ----- generic modify method ---------------------------------- 

(defmacro defmodify (class-name (&rest setf-properties-list) &body body)
  "Defines a MODIFY method on CLASS-NAME.  First, the slots according to SETF-PROPERTY-LIST
are SETF'd.  Then BODY can deal with all properties, bound to the variable PROPERTY-LIST.  
SELF is bound to the object.  Finally, automatically NEXT-METHOD is called.  Redrawing is automatically triggered by
the SETF listeners."
  `(defmethod modify ((self ,class-name) &rest property-list)
     (property-list-setf
       (self property-list)
       ,@(loop for property in setf-properties-list collecting
               `(,property ,(intern (string property) :keyword))))
     ,@body
     (call-next-method-when)
     self))
     
(defgeneric modify (element &rest property-list)
  (:documentation "updates the slots of element according to property list.  Finally the cairo-drawing-area :after
method is called to trigger the redraw."))



;;;; -----------------------------------------------------------
;;;;     create/destroy methods 
;;;; -----------------------------------------------------------

;;;; ---------- create a new element ---------------------------

(defgeneric mk-primitive (cairo-drawing-area object-type &rest initargs)
  (:documentation "Adds the instance OBJECT to the widget CAIRO-DRAWING-AREA"))

(defmacro def-mk-primitive (type (object initargs) &body body)
  "Defines a CREATE method on CAIRO-DRAWING-AREA and on PRIMITIVE.  The method takes a drawing area instance (or a primitive),
a type keyword, and a &rest plist.  The plist is directly passed to MAKE-INSTANCE, thus
it must take all initargs supplied in the call to create.

If the method is called on a primitve, and in the initargs keywords are given with the special value :INHERIT, they
are initilized with a c? inheriting from the primitive.

In BODY, PARENT is bound to
the parent of the newly created object, OBJECT to the newly created object, and initargs to the passed arguments."
  `(progn
     (defmethod mk-primitive ((parent cairo-drawing-area) (type (eql ',(intern (string type) :keyword))) &rest ,initargs)
       (let ((,object (apply #'make-instance (quote ,type) :widget parent ,initargs)))
         (push ,object (.canvas parent))
         ,@body
         (redraw parent)
         ,object))
     (defmethod mk-primitive ((parent primitive) (type (eql ',(intern (string type) :keyword))) &rest ,initargs)
       (let ((,object (apply #'make-instance (quote ,type) :widget (widget parent) :fm-parent parent ,initargs)))
         (deb "creating ~a as a kid of ~a" type parent)
         (with-integrity (:change :add2kids)
	   (push ,object (kids parent))
	   (trc "body")
	   ,@body
	   (trc "schedule redraw")
	   (redraw (widget parent))
	   (trc "done"))
         ,object))
     (defmacro mk-primitive-inherit (parent type (&rest inherited-initargs) &rest initargs)
       `(mk-primitive ,parent
		      ,(intern (string type) :keyword)
		      ,@(loop for inherited in inherited-initargs
			   collecting (intern (string inherited) :keyword)
			   collecting `(c? (,(intern (string inherited)) ,parent)))
		      ,@initargs))))


;;;; ------ destroy methods ----------------------------------------------

(defmethod not-to-be :before ((self cairo-drawing-area))
  (trc "not-to-be cairo-drawing area erasing everything" self)
  (setf (canvas self) nil
	(.canvas self) nil))

(defgeneric remove-primitive (primitive)
  (:documentation "Removes primitive"))

(defgeneric erase (cairo-drawing-area)
  (:documentation "Blanks the widget"))

(defmethod erase ((self cairo-drawing-area))
  (setf (.canvas self) nil)
  (redraw self))

  
;;;; -----------------------------------------------------------
;;;;     positioning methods 
;;;; -----------------------------------------------------------

(defgeneric move (primitive new-anchor-point)
  (:documentation "Moves the primitive such that (anchor-point widget) = new-anchor-point")) 

(defgeneric anchor-point (primitive)
  (:documentation "Returns a significant point for primitive.  Used for move method."))

(defgeneric dock-point (primitive point)
  (:documentation "Returns a point where lines/arcs/arrows from point should attach to primitive.  Defaults to
anchor-point."))



;;;; ================================================================
;;;;      abstract primitves
;;;; ================================================================

;;;; -----------------------------------------------------------
;;;;       base class for all primitives
;;;; -----------------------------------------------------------

(defprimitive primitive (family)
  ((rgb            '(0 0 0))
   (alpha          1)
   (draggable      nil)
   (selectable     nil)
   (dragged-p      nil)
   (mouse-over-p   nil)
   (widget         nil))
  ( :readers ((selected-p (when-bind (w (^widget)) (true (member self (selection w))))))
    :no-redraw
    (draggable dragged-p mouse-over-p selectable selected-p)
    :default-initargs (:widget (c? (bwhen (parent (upper self)) (widget parent))))))

(defmodify primitive (draggable)
  (deb "modify primitive ~a with ~a" self property-list))

(defmethod remove-primitive ((primitive primitive))
  (when (upper primitive)
    (setf (kids (upper primitive)) (remove primitive (kids (upper primitive)))))
  (bwhen (widget (widget primitive))
    (with-accessors ((.canvas .canvas)) widget
     (when (member primitive .canvas)
       (setf .canvas (remove primitive .canvas))))
    (redraw widget))
  nil)

(defobserver .kids ((self primitive))
  (bwhen (widget (widget self)) (redraw widget)))

;;;;  mouse over listener to update the widget 

(defmethod Get-hover ((self cairo-drawing-area) (primitive primitive))
  (with-slot-accessors (hover hover-history) self
                       (when hover (push hover hover-history))
                       (setf hover primitive)))

(defmethod release-hover ((self cairo-drawing-area) (primitive primitive))
  (with-slot-accessors (hover hover-history) self
    (labels ((find-previous () (when-bind (prev (pop hover-history))
				 (if (mouse-over-p prev) prev
				     (find-previous)))))
      (let ((prev (find-previous)))
	(trcx nil "resetting hover from --> to " primitive prev (and prev (mouse-over-p prev)))
	(setf hover prev)))))
              

(defobserver mouse-over-p ((self primitive) new-state)
  (bwhen (widget (widget self))
    (trc nil "mouse-over-p changed" self new-state)
    (if new-state
	(get-hover widget self)
	(release-hover widget self))))


;;;; -----------------------------------------------------------
;;;;       basic geometry 1d/2d
;;;; -----------------------------------------------------------
;;;; ------  point (0 0) --------------------------
    
(defprimitive point-primitive (primitive)
  ((p (2d:v 0 0)))
  (:readers
    ((anchor-point (^p)))))

(defmodify point-primitive (p))

(defmethod move ((self point-primitive) new-anchor-point)
  (setf (p self) new-anchor-point))



;;;; ----- 2d objects -------------------------------

;;;; ------ rectangle (x1 y1) -> (x2 y2) ------------

(defprimitive rectangle-primitive (primitive)
  ((p1 (2d:v 0 0))
   (p2 (2d:v 0 0)))
  (:readers
    ((p-min (2d:v (min (2d:x (^p1)) (2d:x (^p2))) (min (2d:y (^p1)) (2d:y (^p2)))))
     (p-max (2d:v (max (2d:x (^p1)) (2d:x (^p2))) (max (2d:y (^p1)) (2d:y (^p2)))))
     (size (2d:v- (^p-max) (^p-min)))
     (delta (2d:v- (^p2) (^p1)))
     (c-o-g (2d:v+ (^p-min) (2d:v* .5 (^size))))
     (anchor-point (^p1)))
    :no-redraw (p-min p-max size delta c-o-g anchor-point)))

(defmodify rectangle-primitive (p1 p2))

(defmethod move ((self rectangle-primitive) new-anchor-point)
  (setf (p2 self) (2d:v+ (delta self) new-anchor-point)
        (p1 self) new-anchor-point))


;;;; ------- line / area ------------------------------

(defprimitive line-primitive (primitive)
  ((filled nil)
   (stroked t)
   (fill-rgb nil)
   (fill-alpha nil)
   (line-width 2)))

(defdraw line-primitive (rgb alpha fill-rgb fill-alpha line-width stroked filled)
  (cl-cairo2:set-line-width line-width)
  (when stroked
    (cl-cairo2:set-source-rgba (first rgb) (second rgb) (third rgb) alpha)
    (if filled
        (cl-cairo2:stroke-preserve)
        (cl-cairo2:stroke)))
  (when filled
    (let ((fill-rgb (or fill-rgb rgb))
          (fill-alpha (or fill-alpha alpha)))
      (cl-cairo2:set-source-rgba (first fill-rgb) (second fill-rgb) (third fill-rgb) fill-alpha))
    (cl-cairo2:fill-path)))

 
(defmodify line-primitive (rgb alpha fill-rgb fill-alpha line-width filled))

;;;; -----------------------------------------------------------
;;;;       drawable primitives
;;;; -----------------------------------------------------------

;;;; -------- line --------------------------------------------

;; defined by the outer "box" and "line" characteristics
(defprimitive line (rectangle-primitive line-primitive)
  ()
  (:readers
   ((polar (2d:polar-coords (^delta)))
    (mouse-over-p (when (^widget)
		    (with-accessors ((mouse mouse-pos)) (widget self)
		      (when-bind* ((p1 (^p1))
				   (p2 (^p2))
				   (line-width (^line-width)))
		       (and mouse
			    (2d:point-in-box-p mouse p1 p2 :tol line-width)
			    (< (2d:distance-point-line mouse p1 p2) (* line-width 2))))))))
   :no-redraw (polar mouse-over-p)))


(defdraw line (p1 p2)
  (cl-cairo2:move-to (2d:x p1) (2d:y p1))
  (cl-cairo2:line-to (2d:x p2) (2d:y p2)))

(def-mk-primitive line (line initargs))


;;; ------- rectangle -------------------------------------------

(defprimitive rectangle (rectangle-primitive line-primitive)
  ()
  (:readers
   ((mouse-over-p (when (^widget)
		    (with-accessors ((mouse mouse-pos)) (widget self)
		      (and (2d:point-in-box-p mouse (^p1) (^p2) :tol (^line-width))
			   (if (not (^filled))
			       (2d:point-in-box-p mouse (^p1) (^p2) :tol (^line-width))
			       t))))))
   :no-redraw (mouse-over-p)))

(defdraw rectangle (p1 delta)
  (cl-cairo2:rectangle (2d:x p1) (2d:y p1) (2d:x delta) (2d:y delta)))

(def-mk-primitive rectangle (rect initargs))

(defmethod docking-point ((self rectangle) point)
  (let* ((phi (2d:polar-angle (2d:v- point (c-o-g self))))
         (radius (2d:min-abs (unless (2d:~= (mod phi pi) (/ pi 2))
			       (/ (2d:x (size self)) 2 (cos phi)))
			     (unless (2d:~= (mod phi pi) 0)
			       (/ (2d:y (size self)) 2 (sin phi))))))
    (2d:v+ (c-o-g self) (2d:cartesian-coords (2d:v phi (+ radius (line-width self)))))))

;;;; -------- arc --------------------------------------------------

(defprimitive arc (point-primitive line-primitive)
  ((radius 0)
   (start-angle 0)
   (stop-angle (* 2 pi)))
  (:readers
   ((mouse-over-p (when (^widget)
		    (trc nil "calculating mouse-over-p for arc" self)
		    (with-accessors ((mouse mouse-pos)) (^widget)
		      (let ((offset (2d:v- mouse (^p)))
			    (abs-angle-1 (2d:abs-angle (^start-angle)))
			    (abs-angle-2 (2d:abs-angle (^stop-angle))))
			(trcx nil "got" offset (^start-angle) abs-angle-1 (^stop-angle) abs-angle-2)
			(and (< (min abs-angle-1 abs-angle-2)
				(2d:abs-angle (2d:polar-angle offset))
				(max abs-angle-1 abs-angle-2))
			     (< (if (^filled) 0 (- (^radius) (* 2 (^line-width))))
				(2d:polar-radius offset)
				(+ (^radius) (* 2 (^line-width)))))))))
    (c-o-g (^p)))
   :no-redraw (mouse-over-p c-o-g)))

(defmodify arc (radius start-angle stop-angle))

(defdraw arc (p radius start-angle stop-angle)
  (cl-cairo2:arc (2d:x p) (2d:y p) radius start-angle stop-angle))

(def-mk-primitive arc (arc initargs))

(defmethod docking-point ((self arc) point)
  (2d:intersect-line-circle point (p self) (radius self)))
  
;;;; ------- text ------------------------------------------------------

(defprimitive text-label (point-primitive)
  ((text "")
   (font-face "Arial")
   (font-size 12)
   (italic nil)
   (bold nil)
   (alignment :center)
   (vertical-alignment :center)))

(defdraw text-label (p text font-face font-size italic bold alignment vertical-alignment rgb alpha)
  (cl-cairo2:set-font-size font-size)
  (cl-cairo2:select-font-face font-face
                              (if italic :italic :normal)
                              (if bold :italic :normal))
  (multiple-value-bind (x-bearing y-bearing text-width text-height)
      (cl-cairo2:text-extents text)
    (let ((pos-x (- (2d:x p) (case alignment
			       (:left x-bearing)
			       (:center (+ x-bearing (/ text-width 2)))
			       (:right (+ x-bearing text-width))
			       (t (error "unknown alignment ~a.  allowed:  :left, :center, :right" alignment)))))
          (pos-y (- (2d:y p) (case vertical-alignment
			       (:above (+ y-bearing text-height))
			       (:center (+ y-bearing (/ text-height 2))) 
			       (:below y-bearing)
			       (t (error "unknown vertical alignment ~a.  allowed:  :above, :center, :below" vertical-alignment))))))
      (cl-cairo2:set-source-rgba (first rgb) (second rgb) (third rgb) alpha)
      (cl-cairo2:move-to pos-x pos-y)
      (cl-cairo2:show-text text)
      (cl-cairo2:stroke))))

                      
(defmodify text-label (p text font-face font-size italic bold alignment vertical-alignment rgb alpha))

(def-mk-primitive text-label (text-label initargs))


;;;; ------ a path ---------------------------------------------------------------

;;; points: ( (x1 y1) (x2 y2) ...)

(defprimitive path (line-primitive)
  ((points nil)
   (closed nil))
  (:readers
    ((anchor-point (first (^points))))))

(defdraw path (points closed)
  (when points
    (cl-cairo2:move-to (2d:x (first points)) (2d:y (first points)))
    (dolist (point (cdr points))
      (cl-cairo2:line-to (2d:x point) (2d:y point)))
    (when closed
      (cl-cairo2:close-path))))

(defmodify path (points closed))

(def-mk-primitive path (path initargs))

(defmethod move ((self path) new-anchor-point)
  (let ((delta (2d:v- new-anchor-point (first (points self)))))
    (setf (points self) (mapcar #'(lambda (v) (2d:v+ v delta)) (points self)))))

;;;; -------- an arrow -----------------------------------------------------------------

(defprimitive arrow-head (path)
  ((closed t)
   (filled t))
  (:readers ((points (let ((u (upper self))) (list (p2 u) (fin-1 u) (fin-2 u)))))
	    :from-upper (rgb alpha fill-rgb fill-alpha widget)))
   
(defprimitive arrow-line (line)
  ((arrow-angle 15)
   (arrow-length 15))
  (:kids-entries ((make-instance 'arrow-head))
		 :readers  ((arrow-angle-rad (2d:deg->rad (^arrow-angle)))
			    (fin-1 (2d:v+ (^p2)
					  (2d:cartesian-coords (2d:v-polar (+ (2d:phi (^polar)) (^arrow-angle-rad) pi)
									   (^arrow-length)))))
			    (fin-2 (2d:v+ (^p2)
					  (2d:cartesian-coords (2d:v-polar (- (2d:phi (^polar)) (^arrow-angle-rad) pi)
									   (^arrow-length))))))))
(defmodify arrow-line (arrow-angle arrow-length))

(def-mk-primitive arrow-line (self initargs))

;;;; -----------------------------------------------------------
;;;;       event handlers 
;;;; -----------------------------------------------------------

(defun cairo-drawing-area-draw (self)
  (cl-cairo2::with-gtk-context (context (gtk-widget-get-window (id self)))
    (setf (cairo-context self) context)
    (mapcar #'draw (prims self))))

;;;; ------- buttons - clicked ----------------------------------

(defun contains-any (items list)
  (labels ((check (items list) (or (find (car list) items)
				   (and (cdr list) (check items (cdr list))))))
    (check items list)))

(defun cairo-drawing-area-button-press (self button state pos)
  (setf (button-down-position self) pos)
  (case button
    (1
     (trc nil "button down on" (hover self))
     (bif (prim (hover self))
	  ;; prim  --> select/toggle
	  (with-slot-accessors (selection) self
			       (when (selectable prim)
				 (if (contains-any '(:shift :control) state)
				     ;; toggle if ctrl/shift
				     (progn
				       (trc nil "CTRL/SHIFT -- toggeling" prim)
				       (if (selected-p prim)
					   (setf selection (delete prim selection))
					   (push prim selection)))
				     ;; else: only select the current, if not yet selected
				     (unless (selected-p prim)
				       (setf selection (list prim))))))
					;(deb "selection: ~a" selection)))
	  ;; no prim --> draw a select box
	  (progn
	    (trc nil "START SELECT-BOX")
	    (unless (contains-any '(:shift :control) state)
	      (setf (selection self) nil))
	    (setf (select-box self) (mk-primitive self
						  :rectangle
						  :p1 pos
						  :p2 (c? (mouse-pos (^widget)))
						  :rgb (selection-color (^widget))
						  :mouse-over-p (c-in nil)
						  :filled t
						  :fill-rgb (selection-color (^widget))
						  :fill-alpha .1))
	    (trc nil "select box is" (select-box self)))))
    (t (bwhen (box (select-box self))
	 (trc nil "CANCEL SELECT-BOX")
	 (setf box (remove-primitive box)))
       (when (dragging self)
	 (trc nil "CANCEL DRAG")
	 (dolist (prim (selection self))                                  
	   (setf (dragged-p prim) nil))
	 (setf (dragging self) nil
	       (drag-start self) nil
	       (drag-offset self) nil))
       (setf (selection self) nil))))

(defun cairo-drawing-area-button-release (self button state pos)
  (when (= button 1)
    (cond
      ((dragging self)
       ;; this is the button release after a dragging event
       (trc nil "FINISH DRAGGING")
       (with-slot-accessors (dragging on-dragged drag-offset drag-start selection) self
			    (dolist (prim selection)
			      ;; call on-dragged [widget] [button] [primitive] [start-pos] [end-pos]
			      (funcall-when on-dragged
					    self
					    button
					    prim
					    (gethash prim drag-start)
					    (2d:v+ pos (gethash prim drag-offset)))
			      (setf (dragged-p prim) nil))
			    ;; exit dragging mode
			    (setf dragging nil
				  drag-start nil
				  drag-offset nil)))
      ((select-box self)
       (trc nil "FINISH SELECT-BOX")
       (with-slot-accessors (selection prims button-down-position select-box) self
	   (dolist (prim prims)
	     (trc nil "checking" prim)
	     (and (selectable prim)
		  (2d:point-in-box-p (c-o-g prim) button-down-position pos)
		  (push prim selection)
		  (trc nil "--> selected " prim)))
	   (trc nil "selection is now" selection)
	   (setf select-box (remove-primitive select-box))))
      (t (with-slot-accessors (selection hover) self
			      (unless (contains-any '(:shift :control) state)
				(setf selection (list hover))))))
    ;; general button release event
      ;; call on-clicked [widget] [button] [primitive] [position]
    (funcall-when (on-clicked self) self button (hover self) pos)
    (setf (button-down-position self) nil)))


;;;; ------- pointer motion ------------------------------------------------------

(defun cairo-drawing-area-motion (self pos)
  (cond
    ((dragging self)
     (dolist (prim (selection self))
       (when (draggable prim)
	 (move prim (2d:v+ pos (gethash prim (drag-offset self)))))))
    ((bwhen (start-pos (button-down-position self))
       (and (not (select-box self))
	    (> (2d:polar-radius (2d:v- start-pos pos)) (drag-threshold self))))
     (trc nil "START DRAGGING")
     ;; initiate dragging
     (with-slot-accessors (drag-offset drag-start selection dragging) self
	 (setf drag-offset (make-hash-table)
	       drag-start (make-hash-table)
	       dragging t)
	 (dolist (prim (selection self))
	   (when (draggable prim)
	     (setf (dragged-p prim) t
		   (gethash prim drag-offset) (2d:v- (anchor-point prim) pos)
		   (gethash prim drag-start) (anchor-point prim))))))))
 
