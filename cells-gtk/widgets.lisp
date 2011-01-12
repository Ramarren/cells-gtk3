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

(in-package :cgtk)

(defmodel g-object (family)
  ((def-gobject-class-name :accessor def-gobject-class-name :initarg :def-gobject-class-name :initform nil)
   (new-function-name :accessor new-function-name :initarg :new-function-name 
		      :initform (c_1 (intern (format nil "GTK-~a-NEW~a"
						     (def-gobject-class-name self)
						     (or (new-tail self) ""))
					     :gtk-ffi)))
   (new-args :accessor new-args :initarg :new-args :initform nil)
   (new-tail :accessor new-tail :initarg :new-tail :initform nil)
   (callbacks :cell nil :accessor callbacks
	      :initform nil
	      :documentation "assoc of event-name, callback closures to handle widget events")
   (id :initarg :id :accessor id 
       :initform (c? (without-c-dependency
                       (unless (new-function-name self)
                         (error "No constructor specified for type ~a" (type-of self)))
                       (when *gtk-debug* 
                         (trc "NEW ID" (new-function-name self) (new-args self)) (force-output))
                       (let ((id (apply (symbol-function (new-function-name self))
                                        (new-args self))))
                         (gtk-object-store id self)
                         (when (g-object-is-floating id)
                           (g-object-ref-sink id))
                         id))))))

(defmodel gtk-object (g-object)
  ((container :cell nil :initarg :container :accessor container :initform nil))
  (:default-initargs
      :md-name nil ;; kwt: was (c-in nil), but this is not a cell
    :value (c-in nil)))

;; --------- provide id-to-clos lookup ------

;;;
;;; gtk object registry
;;; 

(defvar *gtk-objects* nil)
(defvar *widgets* nil)

(defun gtk-objects-init ()
  (setf *gtk-objects* (make-hash-table :size 100 :rehash-size 100)
	*widgets* (make-instance 'cells-store)))

;;; id lookup

(defun gtk-object-store (gtk-id gtk-object &aux (hash-id (cffi:pointer-address gtk-id)))
  (unless *gtk-objects*
    (gtk-objects-init))
  (bwhen (name (md-name gtk-object))    
    (store-add name *widgets* gtk-object))
  (let ((known (gethash hash-id *gtk-objects*)))
    (cond
     ((eql known gtk-object))
     (t
      #+ssh (when known
	(warn (format nil "Object ~a has been reclaimed by GTK.  Cells-gtk might have stale references" known)))
      (setf (gethash hash-id *gtk-objects*) gtk-object)))))


(defun gtk-object-forget (gtk-id gtk-object)
  (when (and gtk-id gtk-object)
    (trc nil "    forgetting id/obj" gtk-id gtk-object)
    (let ((ptr (cffi:pointer-address gtk-id)))
      (assert *gtk-objects*)
      (remhash ptr *gtk-objects*)
      #+unnecessary (mapc (lambda (k) (gtk-object-forget (slot-value k 'id) k))
			  (slot-value gtk-object '.kids)))   ; unnecessary, ph
    (trc nil "    done" gtk-id gtk-object)))

(defun gtk-object-find (gtk-id &optional must-find-p &aux (hash-id (cffi:pointer-address gtk-id)))
  (when *gtk-objects*
    (let ((clos-widget (gethash hash-id *gtk-objects*)))
      (when (and must-find-p (not clos-widget))
        (format t "~&gtk.object.find> ID ~a not found!!!!!!!" hash-id)
        (maphash (lambda (key value)
                   (format t "~&  known: ~a | ~a" key value))
          *gtk-objects*)
        (gtk-report-error gtk-object-id-error "gtk.object.find ID not found ~a" hash-id))         
      clos-widget)))

;;; name lookup

(defmacro with-widget ((widget name &optional alternative) &body body)
  `(bwhen-c-stored (,widget ,name *widgets* ,alternative)
     ,@body))

(defun find-widget (name &optional default)
  (with-widget (w name default)
    w))

(defmacro with-widget-value ((val name &key (accessor '(quote value)) (alternative nil)) &body body)
  (with-gensyms (widget)
   `(with-widget (,widget ,name ,alternative)
      (bif (,val (funcall (symbol-function ,accessor) ,widget))
	   (progn ,@body)
	   ,alternative))))

(defun widget-value (name &optional default (accessor 'value))
  (with-widget-value (val name :accessor accessor :alternative default)
    val))

;;;
;;; callbacks
;;;

;; ----- fake callbackable closures ------------

(defun callback-register (self callback-key closure)
  (let ((x (assoc callback-key (callbacks self))))
    (if x (rplacd x closure)
      (push (cons callback-key closure) (callbacks self)))))

(defun callback-recover (self callback-key)
  (cdr (assoc callback-key (callbacks self))))

;;;
;;; callback table
;;; 

(defvar *gtk-global-callbacks* nil)

(defun gtk-reset ()
  (cells-reset)
  (gtk-objects-init)
  (setf *gtk-global-callbacks*
	(make-array 128 :adjustable t :fill-pointer 0)))

(defun gtk-global-callback-register (callback)
  (vector-push-extend callback *gtk-global-callbacks* 16))

(defun gtk-global-callback-funcall (n)
  (trc nil "gtk-global-callback-funcall >" n
    *gtk-global-callbacks*
    (when n (aref *gtk-global-callbacks* n)))
  (funcall (aref *gtk-global-callbacks* n)))


(defmethod configure ((self gtk-object) gtk-function value)
  (apply gtk-function
    (id self)
    (if (consp value)
        value
      (list value))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (fboundp 'gtk-function-name)
   (defun gtk-function-name (class option)
     (let ((slot-access (if (atom option)
			    (list 'set option)
			    (append (and (second option) (list (second option)))
				    (list (first option))))))
       (intern (format nil "GTK-~a~{-~a~}" class slot-access) :gtk-ffi)))))

;;; --- widget --------------------
;;; Define handlers that recover the callback defined on the widget

(defmacro def-gtk-event-handler (event)
  `(cffi:defcallback ,(intern (format nil "~a-HANDLER" event)) :int
     ((widget :pointer) (event :pointer) (data :pointer))
     (if-bind (self (gtk-object-find widget))
	  (let ((cb (callback-recover self ,(intern (string event) :keyword))))
	    (funcall cb self widget event data))
	  (trc nil "Unknown widget from prior run. Clean up on errors" widget))))

(def-gtk-event-handler clicked)
(def-gtk-event-handler changed)
(def-gtk-event-handler activate)
(def-gtk-event-handler value-changed)
(def-gtk-event-handler day-selected)
(def-gtk-event-handler selection-changed)
(def-gtk-event-handler toggled)
(def-gtk-event-handler delete-event)
(def-gtk-event-handler destroy-event)
(def-gtk-event-handler modified-changed)
(def-gtk-event-handler select-page)

(defparameter *widget-callbacks*
  (list (cons 'clicked (cffi:get-callback 'clicked-handler))
    (cons 'changed (cffi:get-callback 'changed-handler))
    (cons 'activate (cffi:get-callback 'activate-handler))
    (cons 'value-changed (cffi:get-callback 'value-changed-handler))
    (cons 'day-selected (cffi:get-callback 'day-selected-handler))
    (cons 'selection-changed (cffi:get-callback 'selection-changed-handler))
    (cons 'toggled (cffi:get-callback 'toggled-handler))
    (cons 'delete-event (cffi:get-callback 'delete-event-handler))
    (cons 'destroy-event (cffi:get-callback 'destroy-event-handler))    
    (cons 'modified-changed (cffi:get-callback 'modified-changed-handler))
    (cons 'select-page (cffi:get-callback 'select-page-handler))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (defmacro def-object (&rest args)
    `(def-gtk gtk-object ,@args))
  (defmacro def-widget (&rest args)
    `(def-gtk widget ,@args))  
  (defmacro def-gtk (gtk-superclass class
                      superclasses
                      (&rest std-slots)
                      (&rest gtk-slots) (&rest gtk-signals) &rest defclass-options)
    
    (multiple-value-bind (slots outputs)
        (loop for gtk-option-def in gtk-slots
            for slot-name = (if (atom gtk-option-def)
			       gtk-option-def (car gtk-option-def))
	   collecting `(,slot-name :initform (c-in nil)
                          :initarg ,(intern (string slot-name) :keyword)
                          :accessor ,slot-name)
            into slot-defs
            collecting `(defobserver ,slot-name ((self ,class))
                          (when (or new-value old-value)
                            #+shhh (when *gtk-debug*
                              (TRC ,(format nil "output before ~a-~a" class slot-name) new-value) (force-output))
                            (configure self #',(gtk-function-name class (if (and (listp gtk-option-def) (cdr gtk-option-def))
									    (second gtk-option-def)
									    gtk-option-def))
                              new-value)
                            #+shhh (when *gtk-debug*
                              (TRC ,(format nil "output after ~a-~a" class slot-name) new-value) (force-output))))
	   into outputs
	   finally (return (values slot-defs outputs)))
      (multiple-value-bind (signals-slots signals-outputs)
          (loop for signal-slot in gtk-signals
	     for slot-name = (intern (format nil "ON-~a" signal-slot))
	     collecting `(,slot-name :initform (c-in nil)
                            :initarg ,(intern (string slot-name) :keyword)
                            :accessor ,slot-name)
              into signals-slots-defs
              collecting `(defobserver ,slot-name ((self ,class))
                            (when new-value
                              (callback-register self
                                ,(intern (string signal-slot) :keyword)
                                new-value)
                              (let ((cb (cdr (assoc ',signal-slot *widget-callbacks*))))
                                (assert cb () "Callback ~a not defined in *widget-callbacks*" ',signal-slot)
                                #+shhtk (trc nil "in defobserver gtk-signal-connect pcb:"
                                  cb ',slot-name (id self))
                              (gtk-signal-connect (id self)
                                ,(string-downcase (string signal-slot)) cb))))
              into signals-outputs-defs
              finally (return (values signals-slots-defs signals-outputs-defs)))
        (let ()
          `(progn
             (defmodel ,class ,(or superclasses (list gtk-superclass))
               (,@(append std-slots slots signals-slots))
               (:default-initargs
                   :def-gobject-class-name ',class
                 ,@defclass-options))
             (eval-now!
               (export ',class))
             (eval-now!
               (export ',(mapcar #'first (append std-slots slots signals-slots))))
             (defun ,(intern (format nil "MK-~a" class)) (&rest inits)
               (when *gtk-debug* (trc nil "MAKE-INSTANCE" ',class) (force-output))
               (apply 'make-instance ',class
                 (append inits (list :fm-parent (when (boundp '*parent*) *parent*))) ;; cells3 needs parent up front
                 ))
             (eval-now!
               (export ',(intern (format nil "MK-~a" class))))
             ,@outputs
             ,@signals-outputs))))))

(defmacro callback ((widg event data) &body body)
  `(lambda (self ,widg ,event ,data) 
     (declare (ignorable self ,widg ,event ,data))
     ;(print (list :anon-callback self ,widg ,event ,data))
     (prog1
         (progn
           ,@body
           1) ;; a boolean which indicates, IIRC, "handled"
       #+shhh (print (list :callback-finis self ,widg ,event ,data)))))

(defmacro callback-if (condition (widg event data) &body body)
  `(c? (and ,condition
         (lambda (self ,widg ,event ,data) 
                   (declare (ignorable self ,widg ,event ,data))
                   ;(print (list :anon-callback-if self ,widg ,event ,data))
                   ,@body
                   1))))

(cffi:defcallback timeout-handler-callback :int ((data :pointer))
  (let* ((id (cffi:mem-aref data :int 0))
	 (r2 (gtk-global-callback-funcall id)))
    (trc nil "timeout func really returning" r2)
    (if r2 1 0)))

(defun timeout-add (milliseconds function)
  "Call FUNCTION repeatedly, waiting MILLISECONDS between calls. 
   Stops calling when function return false."
  (let* ((id (gtk-global-callback-register
              (lambda ()
                ;;(print :timeout-add-global)
                (let ((r (with-gdk-threads
			  (funcall function))))
                  (trc nil "timeout func returning" r)
                  r))))
	 (c-id (cffi:foreign-alloc :int :initial-element id)))
    (trc nil "timeout-add > passing cb data, *data" c-id (cffi:mem-aref c-id :int 0))
    (g-timeout-add (floor milliseconds) (cffi:get-callback 'timeout-handler-callback) c-id)))

(def-object widget ()
  ((tooltip :accessor tooltip :initarg :tooltip :initform (c-in nil))
   (popup :owning t :accessor popup :initarg :popup :initform (c-in nil))
   (visible :accessor visible :initarg :visible :initform (c-in t))
   (sensitive :accessor sensitive :initarg :sensitive :initform (c-in t))
   (expand :accessor expand? :initarg :expand :initform nil)
   (x-expand :accessor x-expand :initarg :x-expand :initform (c? (expand? self)))
   (y-expand :accessor y-expand :initarg :y-expand :initform (c? (expand? self)))
   (fill :accessor fill? :initarg :fill :initform nil)
   (x-fill :accessor x-fill :initarg :x-fill :initform (c? (fill? self)))
   (y-fill :accessor y-fill :initarg :y-fill :initform (c? (fill? self)))
   (padding :accessor padding? :initarg :padding :initform 2)
   (x-pad :accessor x-pad :initarg :x-pad :initform (c? (padding? self)))
   (y-pad :accessor y-pad :initarg :y-pad :initform (c? (padding? self)))
   (width :accessor width :initarg :width :initform nil)
   (height :accessor height :initarg :height :initform nil)
   (allocated-width :accessor allocated-width :initform (c-in 0))
   (allocated-height :accessor allocated-height :initform (c-in 0))
   )
  ()
  (focus show hide delete-event destroy-event)
  ;; this is called unless the user overwrites this routine
  )

(cffi:defcallback reshape-widget-handler :int ((widget :pointer) (event :pointer) (data :pointer))
  (declare (ignore data event))
  (trc nil "reshape" widget)
  (bwhen (self (gtk-object-find widget))
    (multiple-value-bind (new-width new-height) (gtk-widget-size widget)
      (trc nil "reshape widget to new size" self widget new-width new-height)
      (with-integrity (:change :adjust-widget-size)
	(setf (allocated-width self) new-width
	      (allocated-height self) new-height))))
  0)

(defmethod md-awaken :after ((self widget))
  (gtk-signal-connect-swap (id self) "configure-event" (cffi:get-callback 'reshape-widget-handler) :data (id self)))

(defmethod focus ((self widget))
  (gtk-widget-grab-focus (id self)))

(defobserver width ((self widget))
  (when new-value
    (gtk-widget-set-size-request (id self)
				 new-value
				 (or (height self) -1))))

(defobserver height ((self widget))
  (when new-value
    (gtk-widget-set-size-request (id self)
				 (or (width self) -1)
				 new-value)))

(defobserver sensitive ((self widget))
  (gtk-widget-set-sensitive (id self) new-value))
  

(defobserver popup ((self widget))
  (when new-value
    (gtk-widget-set-popup (id self) (id new-value))))
    
(defobserver visible ((self widget))
  (if new-value 
      (gtk-widget-show (id self))
    (gtk-widget-hide (id self))))

(defmethod not-to-be :around ((self g-object))
  (trc nil "g-object not-to-be :around" (md-name self) self)
  (trc nil "  store-remove")
  (when (eql (store-lookup (md-name self) *widgets*) self)
    (store-remove (md-name self) *widgets*))
  (trc nil "  object-forget")
  (gtk-object-forget (id self) self)
  (when  *gtk-debug*
    (trc "OBJECT UNREF" (slot-value self '.md-name) (type-of self) self)
    (force-output))
  (g-object-unref (id self))

  (trc nil "  call-next-method")
  (call-next-method)
  (trc nil "  done"))

(defmethod not-to-be :around ((self widget))
  (trc nil "  widget-destroy")
  (when  *gtk-debug*
    (trc t "WIDGET DESTROY" (slot-value self '.md-name) (type-of self) self)
    (force-output))
  ;; it will be unrefed in call-next-method
  (g-object-ref (id self))
  ;; this also drops one reference
  (gtk-widget-destroy (slot-value self 'id))
  ;; and call-next-method will drop the last one
  (call-next-method)
  (trc nil "  done"))


(defun assert-bin (container)
  (assert (null (rest (kids container))) 
	  ()
	  "~a is a bin container, must have only one kid" container))

(def-widget container ()
  ()
  (border-width resize-mode)
  ())

(def-widget item (container)
  ()
  ()
  ())

(def-widget window (container)
  ((wintype :accessor wintype :initarg wintype :initform 0)
   (title :accessor title :initarg :title
     :initform (c? (string (class-name (class-of self)))))
   (icon :initarg :icon :accessor icon :initform nil)
   (decorated :accessor decorated :initarg :decorated :initform (c-in t))
   (position :accessor set-position :initarg :position :initform (c-in nil))
   (accel-group :accessor accel-group :initform (gtk-accel-group-new))
   (cb-quit-id :initform nil))
  (default-size resizable
   (maximize) (unmaximize) (iconify) (deiconify) (fullscreen) (unfullscreen))
  ()
  :new-args (c_1 (list (^wintype)))
  :resizable t)

(defobserver resizable ((self window))
  (gtk-window-set-resizable (id self) new-value))

(defobserver width ((self window))
  (when new-value
    (gtk-window-set-default-size (id self)
				 new-value
				 (or (height self) -1))))

(defobserver height ((self window))
  (when new-value
    (gtk-window-set-default-size (id self)
				 (or (width self) -1)
				 new-value)))

(defobserver accel-group ((self window))
  (when new-value
    (gtk-window-add-accel-group (id self) new-value)))

(defobserver title ((self window))
  (when new-value
	(gtk-window-set-title (id self) new-value)))

(defobserver icon ((self window))
  (when new-value
    (gtk-window-set-icon-from-file (id self) new-value +c-null+)))

(defobserver decorated ((self window))
  (gtk-window-set-decorated (id self) new-value))

(defobserver position ((self window))
  (when new-value
    (gtk-window-set-position (id self)
      (ecase new-value
	(:none 0)
	(:center 1)
	(:mouse 2)
	(:center-always 3)
	(:center-on-parent 4)))))

(defobserver .kids ((self window))
  (assert-bin self)
  (dolist (kid new-value)
    ;    (when *gtk-debug* (format t "~% window ~A has kid ~A" self kid))
    (when *gtk-debug* (trc "WINDOW ADD KID" (md-name self) (md-name kid)) (force-output))
    (gtk-container-add (id self) (id kid))))

(def-widget event-box (container)
  ((visible-window :accessor visible-window :initarg :visible-window :initform nil))
  (above-child)
  ()
  :above-child t)

(defobserver visible-window ((self event-box))
  (gtk-event-box-set-visible-window (id self) new-value))
  
(defobserver .kids ((self event-box))
  (assert-bin self)
  (when new-value
    (gtk-container-add (id self) (id (first new-value)))))

(declaim (inline widget-id))
(defun widget-id (widget)
  (id widget))

