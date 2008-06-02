 

(in-package :cgtk)

(defvar *gl-config* nil)

;;;
;;; gl drawing area
;;; 

;;;
;;; OpenGL interaction
;;;

(defun get-gl-config ()
  (let ((cfg (gdk-gl-config-new-by-mode '(:gdk-gl-mode-rgba :gdk-gl-mode-depth :gdk-gl-mode-double))))
    (if (cffi-sys:null-pointer-p cfg)
	(let ((cfg (gdk-gl-config-new-by-mode '(:gdk-gl-mode-rgba :gdk-gl-mode-depth :gdk-gl-mode-double))))
	  (warn "No double buffered visual found.  Trying single-buffered.")
	  (if (cffi-sys:null-pointer-p cfg)
	      (error "No OpenGL capable visual found.")
	      cfg))
	cfg)))

(defun gl-init ()
  (gtk-gl-init +c-null+ +c-null+)
  (glut:init)
  (setf *gl-config* (get-gl-config)))


(defmacro with-gl-context ((widget &key (swap-buffers-p t)) &rest body)
  (with-gensyms (drawable context swap-p w wid)
    `(let ((,swap-p ,swap-buffers-p)
	   (,w ,widget))
       (let ((,wid (id ,w)))
	 (let ((,context (gtk-widget-get-gl-context ,wid))
	       (,drawable (gtk-widget-get-gl-window ,wid)))
	   (if (gdk-gl-drawable-gl-begin ,drawable ,context)
	       (progn
		 ,@body
		 (when ,swap-p
		   (when (gdk-gl-drawable-is-double-buffered ,drawable)
		     (trc "swapping buffers")
		     (gdk-gl-drawable-swap-buffers ,drawable)))
		 (gdk-gl-drawable-gl-end ,drawable))
	       (trc "gl-begin failed" ,w ,drawable ,context)))))))

;;;
;;; Event handling
;;;

(defun %gl-draw (self)
  (bwhen (draw-fn (draw self))
   (with-gl-context (self)
     (funcall draw-fn self))))

(cffi:defcallback realize-handler :void ((widget :pointer) (data :pointer))
  (declare (ignore data))
  (let ((self (gtk-object-find widget)))
    (trc "gl realize" self widget (id self))
    (bwhen (init-fn (init self))
      (with-gl-context (self)
	(funcall init-fn self)))
    (trc "done gl realize" self)))


(defun %resize (self)
  (let ((width (allocated-width self))
	(height (allocated-height self)))
    (when (and (plusp width) (plusp height))
      (trc "%resize to" width height)
      (with-gl-context (self)
	(gl:viewport 0 0 width height)

	;; set projection to account for aspect
	(gl:matrix-mode :projection)
	(gl:load-identity)
	(glu:perspective 90 (/ width height) 0.5 20) ; 90 degrees field of view y, clip 0.5-20 z
   
	;; set modelview to identity
	(gl:matrix-mode :modelview)
	(gl:load-identity)
	
	(bwhen (resize-fn (resize self))
	  (funcall resize-fn self))))))

;;;
;;; Widget
;;;

(defmodel gl-drawing-area (drawing-area)
  ((draw :accessor draw :initarg :draw :cell nil :initform nil)
   (init :accessor init :initarg :init :cell nil :initform nil)
   (resize :accessor resize :initarg :resize :cell nil :initform nil))
  (:default-initargs
      :on-draw #'%gl-draw))

(defmethod initialize-instance :after ((self gl-drawing-area) &rest initargs)
  (declare (ignore initargs))
  (trc "registering handlers for" self)
  (gtk-signal-connect-swap (id self) "realize" (cffi:get-callback 'realize-handler) :data (id self))
  (trc "set gl capability" self)
  (gtk-widget-set-gl-capability (id self) *gl-config* +c-null+ t :gdk-gl-rgba-type))

(defobserver allocated-width ((self gl-drawing-area))
  (%resize self))

(defobserver allocated-height ((self gl-drawing-area))
  (%resize self))


;;;
;;; supporting macros
;;;

(export! with-matrix-mode)

(defmacro with-matrix-mode ((mode) &body body)
  `(progn
     (gl:matrix-mode ,mode)
     (gl:load-identity)
     ,@body
     (gl:matrix-mode :modelview)
     (gl:load-identity)))
