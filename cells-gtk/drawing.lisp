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

(in-package :gtk-ffi)

(cffi:defcallback drawing-expose-event-handler :int
  ((drawing-area :pointer) (signal :pointer) (data :pointer))
  (declare (ignorable data signal gkd-event-expose widget))
  (when-bind (self (gtk-object-find drawing-area))
    (init-graphics-context drawing-area)
    (gdk-draw-drawable
     *window*
     *gcontext*
     (funcall (funcall (intern "draw-fn" :cells-gtk) self) self)
     0 0 0 0 -1 -1)
    0))

(cffi:defcallback drawing-button-events-handler :int
  ((drawing-area :pointer) (signal :pointer) (data :pointer))
  (declare (ignorable data))
  (when-bind (self (gtk-object-find drawing-area))
    (let ((event (gdk-event-button-type signal)))
      (when (and (eql (event-type event) :button_press)
                 (= (gdk-event-button-button signal) 1))
        (setf (button1-down self)
              (cons (truncate (gdk-event-button-x signal))
                    (truncate (gdk-event-button-y signal)))))
      (when (and (eql (event-type event) :button_release)
                 (= (gdk-event-button-button signal) 1))
        (setf (button1-down self) nil))))
  0)


(cffi:defcallback drawing-pointer-motion-handler :int
  ((drawing-area :pointer) (signal :pointer) (data :pointer))
  (declare (ignorable data signal widget))
  (when-bind (self (gtk-object-find drawing-area))
    (when-bind (button1 (button1-down self))
      (let ((dx (- (truncate (gdk-event-motion-x signal)) (car button1)))
            (dy(- (truncate (gdk-event-motion-y signal)) (cdr button1))))
        (loop for drawable being the hash-value of (gobjects self) do
             (gdk-draw-drawable *window* *gcontext* drawable 0 0 dx dy -1 -1)))))
  0)


(defun gtk-drawing-set-handlers (widget data)
  (gtk-signal-connect-swap widget "button-press-event"
			   (cffi:get-callback 'drawing-button-events-handler)
			   :data data)
  (gtk-signal-connect-swap widget "button-release-event"
			   (cffi:get-callback 'drawing-button-events-handler)
			   :data data)
  (gtk-signal-connect-swap widget "motion-notify-event"
			   (cffi:get-callback 'drawing-pointer-motion-handler)
			   :data data)
  (gtk-signal-connect-swap widget "expose-event"
			   (cffi:get-callback 'drawing-expose-event-handler)
			   :data data))

;;;============================================================================
(in-package :cgtk)
;;;============================================================================

(def-widget drawing-area ()
  ((draw-fn :accessor draw-fn :initform (c-in nil) :initarg :draw-fn)
   (gobjects :cell nil :initform (make-hash-table :test #'equal) :accessor gobjects)
   (backing-pixmap :cell nil :initform nil)
   (button1-down :cell nil :initform nil :accessor button1-down)
   (button2-down :cell nil :initform nil :accessor button2-down)
   (button3-down :cell nil :initform nil :accessor button3-down))
  () ; gtk-slots
  (expose-event)) ; signal-slots

;;; POD This is essentially an initialize-instance, but run later.
(def-c-output draw-fn ((self drawing-area))
  (gtk-widget-add-events (id self) 772) ; 512 + 256 + 4 button_press, release, motion, 
  (gtk-ffi:gtk-drawing-set-handlers (id self) (id self))) ; Could I arrange for data to be a pixmap or widget ???

(defvar *colors* (make-hash-table :test #'equal))
(defvar *gcontext* nil "The current graphics context")
(defvar *widget-id* nil "The current widget (set in with-pixmap)")
(defvar *window* nil "The current window of the current widget (useful in drawing text.)")

(declaim (inline get-color))
(defun get-color (color-string)
  (or (gethash color-string *colors*)
      (error "No such color: ~A" color-string)))

(defun init-graphics-context (widget)
  (loop for color in '("black" "white" "red" "green" "blue") ; see /usr/X11R6/lib/X11/rgb.txt
	for cobj = (gtk-adds-color-new) do 
	(unless (= 1 (gdk-color-parse color cobj)) (error "unknown color: ~A" color))
	(setf (gethash color *colors*) cobj))
  (setf *gcontext* (gdk-gc-new (gtk-adds-widget-window widget)))
  (gdk-gc-set-rgb-fg-color *gcontext* (get-color "black"))
  (gdk-gc-set-rgb-bg-color *gcontext* (get-color "white"))
  (gdk-gc-set-line-attributes *gcontext* 3 0 0 0))

;(declaim (inline line-style))
(defun line-style (enum)
  (ecase enum
    (:gdk-line-solid 0)
    (:gdk-line-on-off-dash 1)
    (:gdk-line-double-dash 2)))

;(declaim (inline cap-style))
(defun cap-style (enum)
  (ecase enum
    (:gdk-cap-not-last 0)
    (:gdk-cap-butt 1)
    (:gdk-cap-round 2)
    (:gdk-cap-projecting 3)))

;(declaim (inline join-style))
(defun join-style (enum)
  (ecase enum
    (:gdk-join-miter 0)
    (:gdk-join-round 1)
    (:gdk-join-bevel 2)))

;;; POD If I want to free gcs, I'll need g-object-ref. 
;;; This should probably revert to the /previous/ values, not default values!
(defmacro with-gc ((drawable &key (fg "black") (bg "white") (line-width 2) 
		   (line-style :gdk-line-solid) (cap-style :gdk-cap-not-last) (join-style :gdk-join-miter))
		   &body body)
  (with-gensyms (holdgc)
   `(let (,holdgc)
      (unwind-protect
	  (progn
	    (setf ,holdgc *gcontext*)
	    (setf *gcontext* (gdk-gc-new ,drawable))
	    (gdk-gc-set-rgb-fg-color *gcontext* (get-color ,fg))
	    (gdk-gc-set-rgb-bg-color *gcontext* (get-color ,bg))
	    (gdk-gc-set-line-attributes *gcontext* ,line-width (line-style ,line-style)
					(cap-style ,cap-style) (join-style ,join-style))
	    ,@body)
	(setf *gcontext* ,holdgc)))))

(defmacro with-pixmap ((var name &key widget width height) &body body)
  (with-gensyms (new)
     `(let ((,var 
	     (or (when ,name (gethash ,name (gobjects ,widget))) ; no name, force an 'unregistered' one.
		 (progn
		   (setf ,new t)
		   (setf (gethash ,name (gobjects ,widget))
			 (gdk-pixmap-new (setf *window* 
					       (gtk-adds-widget-window 
						(setf *widget-id* (id ,widget)))) ,width ,height -1))))))
	(when ,new (with-gc (,var :fg "white") (draw-rectangle ,var 0 0 ,width ,height t)))
     ,@body)))

(declaim (inline draw-line))
(defun draw-line (drawable x1 y1 x2 y2)
  (gdk-draw-line drawable *gcontext* x1 y1 x2 y2))

;;; POD These are in libgobject, not libg, gobject, I haven't used yet.
(defun g-object-ref (obj) obj)
(defun g-object-unref (obj) obj)

(defmethod register-gobject ((self drawing-area) obj key)
  (setf (gethash key (gobjects self)) obj))

;;;---------------------------------------------------------------------
;;; Drawing primitives
;;;---------------------------------------------------------------------
;;; The idea of these is that the object created is contained in the argument DRAWABLE
;;; (a pixmap, or the :bp (short for :backing-pixmap)). The object drawn will itself be
;;; registered (as NAME) if NAME is non-nil. Note that if you are drawing
;;; into a pixmap other than :bp, you typically would not register the
;;; object drawn, because the pixmap retains the information necessary to redraw itself.
;;; (redrawing after an expose event, or after dragging some object with the pointer
;;; is what this is all about). The code does however keep record of the parent
;;; (it doesn't assume it is :bp) so if you do register an object whose
;;; parent is not :bp, it should still be drawn in the right parent. 

#| WIP
(defun draw-text (name drawable text x y)
  (let* ((gc *gcontext*)
	 (fn (setf name (gobjects self)
		   #'(lambda (x y)
		       (let ((layout (gtk-widget-create-pango-layout *widget-id* text)))
			 (gdk-draw-layout drawable gc x y layout))))))
    (funcall fn x y)))
|#

;(declaim (inline draw-text))
(defun draw-text (drawable text x y)
  "Must be used where *widget-id* and *gcontext* are bound, like in with-pixmap"
  (let (layout)
    (unwind-protect ; hmmm... do I really want this?
	(progn 
	  (setf layout (gtk-widget-create-pango-layout *widget-id* text))
	  (g-object-ref layout)
	  (gdk-draw-layout drawable *gcontext* x y layout))
      (g-object-unref layout)))) 

;(declaim (inline draw-rectangle))
(defun draw-rectangle (drawable x y width height &optional fill-p)
  (gdk-draw-rectangle drawable *gcontext* fill-p x y width height))

;;; POD These should track the placement.... for redraw.... do it in cgtk:drawing-area.
;(declaim (inline insert-pixmap))
(defun insert-pixmap (pixmap x y)
  (gdk-draw-drawable *window* *gcontext* pixmap 0 0 x y -1 -1))

(defvar *my-pixmap* nil)


