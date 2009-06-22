
#|

Cells-GTK drawable

|#


(in-package :cgtk)


;;; 
;;; debug facility
;;; 

(defparameter *debug* t)

(defmacro deb (format-string &rest params)
  `(when *debug*
     (format t ,(concatenate 'string "~&" format-string "~%") ,@params)
     (force-output)))


;;; 
;;; drawing-area -- a canvas widget
;;;  

(def-widget drawing-area ()
  ((mouse-pos     :accessor mouse-pos     :cell t   :initform (c-in (2d:v 0 0)))
   (canvas        :accessor canvas        :cell t   :initform (c-in nil) :initarg :canvas :owning t)
   ; (on-draw self)
   (on-draw :accessor on-draw :cell nil :initarg :on-draw :initform nil)
   ;n/a 
   (on-clicked    :accessor on-clicked    :cell nil :initform nil :initarg :on-clicked)
   ;(on-pressed self button pos)
   (on-pressed    :accessor on-pressed    :cell nil :initform nil :initarg :on-pressed)
   ;(on-released self button pos)
   (on-released    :accessor on-released    :cell nil :initform nil :initarg :on-released)
   ;(on-moved self pos)
   (on-moved      :accessor on-moved      :cell nil :initform nil :initarg :on-moved)   
   )
   () ; gtk-slots
   (expose-event))


;;;  expose

(cffi:defcallback drawing-expose-handler :int
    ((gtk-pointer :pointer) (signal :pointer) (data :pointer))
  (declare (ignorable data signal))
  (trc nil "drawing"  gtk-pointer (gtk-object-find gtk-pointer) data (gtk-object-find data)) 
  (when-bind* ((self (gtk-object-find gtk-pointer))
	       (on-draw (on-draw self)))
    (funcall on-draw self))
  0)


;;; pointer-motion

(cffi:defcallback drawing-pointer-motion-handler :int  ((drawing-area :pointer) (signal :pointer) (data :pointer))
  (declare (ignorable data signal))
  (trc nil "pointer-motion"  drawing-area (gtk-object-find drawing-area) data (gtk-object-find data))
  (when-bind (self (gtk-object-find drawing-area))
    (let ((pos (2d:v (gdk-event-motion-x signal)
		     (gdk-event-motion-y signal))))
      (setf (mouse-pos self) pos)
      (when-bind (on-moved (on-moved self))
	(funcall on-moved self pos))))
  0)


(define-constant +gdk-modifiers+ '((:shift . 0) (:alt . 28) (:control . 2)))

(defun gdk-modifier (state modifier)
  "checks state for the modifiers :shift :alt :control"
  (/= 0 (logand state (ash 1 (cdr (assoc modifier +gdk-modifiers+))))))

(defun gdk-modifiers (state)
  "returns a list of modifiers as keywords"
  (loop for (key . code) in +gdk-modifiers+
       if (/= 0 (logand state (ash 1 code))) collect key))

#|  
  from http://library.gnome.org/devel/gdk/stable/gdk-Windows.html#GdkModifierType
  GDK_SHIFT_MASK    = 1 << 0,
  GDK_LOCK_MASK	    = 1 << 1,
  GDK_CONTROL_MASK  = 1 << 2,
  GDK_MOD1_MASK	    = 1 << 3,
  GDK_MOD2_MASK	    = 1 << 4,
  GDK_MOD3_MASK	    = 1 << 5,
  GDK_MOD4_MASK	    = 1 << 6,
  GDK_MOD5_MASK	    = 1 << 7,
  GDK_BUTTON1_MASK  = 1 << 8,
  GDK_BUTTON2_MASK  = 1 << 9,
  GDK_BUTTON3_MASK  = 1 << 10,
  GDK_BUTTON4_MASK  = 1 << 11,
  GDK_BUTTON5_MASK  = 1 << 12,
  GDK_SUPER_MASK    = 1 << 26,
  GDK_HYPER_MASK    = 1 << 27,
  GDK_META_MASK     = 1 << 28,|#


;;; button-press/release

(cffi:defcallback drawing-button-handler :int ((drawing-area :pointer) (signal :pointer) (data :pointer))
  (declare (ignorable data))
  (when-bind (self (gtk-object-find drawing-area))
    (let ((pos (2d:v (truncate (gdk-event-button-x signal))
		     (truncate (gdk-event-button-y signal))))
	  (button (gdk-event-button-button signal))
	  (state (gdk-event-button-state signal)))
      (case (gdk-event-button-type signal)
	(:button_press
	 (when-bind (on-pressed (on-pressed self))
	   (funcall on-pressed self button (gdk-modifiers state) pos)))
	(:button_release
	 (when-bind (on-released (on-released self))
	   (funcall on-released self button (gdk-modifiers state) pos))))))
  0)


(defmethod initialize-instance :after ((self drawing-area) &rest rest)
  (declare (ignore rest))
  (let ((widget (id self)))
    (trc "registering handlers for" widget)
    (gtk-widget-add-events widget 772) ; 512 + 256 + 4 button_press, release, motion
    (gtk-signal-connect-swap widget "button-press-event" (cffi:get-callback 'drawing-button-handler) :data widget)
    (gtk-signal-connect-swap widget "button-release-event" (cffi:get-callback 'drawing-button-handler) :data widget)
    (gtk-signal-connect-swap widget "motion-notify-event" (cffi:get-callback 'drawing-pointer-motion-handler) :data widget)
    (gtk-signal-connect-swap widget "expose-event" (cffi:get-callback 'drawing-expose-handler) :data widget)))


;;;
;;; redraw method (called to trigger a refresh) 
;;;

;;; a handler if redraw called on nil
(export! redraw)

(defmethod redraw (self))

(defmethod redraw ((self drawing-area))
  "Queues a redraw with GTK."
  (trc nil "queue redraw" self)
  (unless (mdead self)
   (gtk-widget-queue-draw (id self))))



