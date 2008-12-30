(in-package :test-gtk)

;;;
;;; auxiliary color funcs
;;;

(defun highlight-col (rgb)
  (mapcar #'(lambda (val) (min 1 (+ val .3))) rgb))

(defun select-col (rgb)
  (mapcar #'(lambda (val) (max 0 (- val .3))) rgb))

(defmacro rgb? (rgb)
  (with-gensyms (col)
    `(c?
       (let ((,col ,rgb))
	(cond
	  ((mouse-over-p self) (highlight-col ,col))
	  ((selected-p self) (select-col ,col))
	  (t ,col))))))

(defmacro alpha? (alpha)
  (with-gensyms (a)
    `(c? (let ((,a ,alpha))
	   (cond
	     ((dragged-p self) .3)
	     (t ,a))))))

;;;
;;; random generators
;;;

(defun rnd (min max)
  (+ min (random max)))

(defun random-point (min-x min-y max-x max-y)
  (2d:v (rnd min-x max-x) (rnd min-y max-y)))

(defun random-color ()
  (loop for i from 0 below 3 collect (random 1.0)))

;;;
;;; drag'n'drop test
;;;

(defmodel test-cairo-dragging (hbox)
  ()
  (:default-initargs
      :fill t :expand t
      :kids (kids-list?
	     (make-instance 'cairo-drawing-area :md-name :draw :expand t :fill t
			    :fm-parent *parent*
			    :width 500 :height 500)
	     (mk-vbox
	      :kids (kids-list?
		     (list
		      (mk-button :label "Draw Box"
				 :on-clicked (callback (w e d)
					       (let* ((p1 (random-point 10 10 480 480))
						      (p2 (2d:v+ p1 (random-point 10 10 40 40)))
						      (col1 (random-color))
						      (col2 (random-color)))
						 (trcx "rect" p1 p2 col1 col2)
						 (mk-primitive (find-widget :draw) :rectangle
							       :p1 (c-in p1)
							       :p2 (c-in p2)
							       :rgb (rgb? col1)
							       :fill-rgb (rgb? col2)
							       :alpha (alpha? 1)
							       :filled t
							       :draggable t
							       :selectable t))))
		      (mk-button :label "Draw Arc"
				 :on-clicked (callback (w e d)
					       (let* ((p (random-point 10 10 480 480))
						      (radius (rnd 10 40))
						      (col1 (random-color))
						      (col2 (random-color)))
						 (mk-primitive (find-widget :draw) :arc
							       :p (c-in p)
							       :radius (c-in radius)
							       :rgb (rgb? col1)
							       :fill-rgb (rgb? col2)
							       :alpha (alpha? 1)
							       :filled t
							       :draggable t
							       :selectable t))))))))))

;;;
;;; declarative drawing
;;;

(defmodel test-cairo-kids (hbox)
  ()
  (:default-initargs
      :expand t :fill t
      :kids (kids-list?
	     (make-instance
	      'cairo-drawing-area
	      :md-name :draw-sun :expand t :fill t :width 500 :height 500
	      :fm-parent *parent*
	      :canvas (c?n (let ((draw self))
			    (declare (ignorable draw))
			    (list
			     (make-instance
			      'arc
			      :widget self
			      :radius (c? (widget-value :sun-radius 20))
			      :rgb '(0 0 0)
			      :line-width 2
			      :fill-rgb '(1 1 0)
			      :filled t
			      :p (c-in (2d:v 250 250))
			      :draggable t
			      :selectable t
			      :kids (kids-list?
				     (let* ((num (widget-value :sun-rays 18))
					    (r (widget-value :sun-radius 20))
					    (l (widget-value :sun-ray-length 20))
					    (delta (/ #.(* pi 2) num)))
				       (loop for i from 0 below num
					  for phi = 0 then (+ phi delta)
					  collect
					  (let ((phi phi))
					    (make-instance
					     'line
					     :fm-parent *parent*
					     :widget (widget self)
					     :p1 (c? (2d:v+ (p (upper self))
							    (2d:cartesian-coords (2d:v-polar phi (* r 1.2)))))
					     :p2 (c? (2d:v+ (p (upper self))
							    (2d:cartesian-coords (2d:v-polar phi (+ (* r 1.2) l)))))
					     :rgb '(1 1 0)
					     :line-width 3
					     :selectable nil))))
				     ))))
			  ))
	     (mk-vbox
	      :kids (kids-list?
		     (mk-label :text "Radius of the sun:")
		     (mk-spin-button :md-name :sun-radius :init 40 :min 5 :max 200)
		     (mk-label :text "Number of rays:")
		     (mk-spin-button :md-name :sun-rays :init 10 :min 1 :max 30)
		     (mk-label :text "Length of rays")
		     (mk-spin-button :md-name :sun-ray-length :init 50 :min 0 :max 100))))))

;;;
;;; the dialog
;;;

(defmodel test-cairo-drawing (notebook)
  ((new-prim :accessor new-prim :initform (c-in nil)))
  (:default-initargs
      :md-name :test-drawing
    :tab-labels '("Dragging" "Declarative Drawing")
    :kids (kids-list?
	   (make-instance 'test-cairo-dragging :fm-parent *parent*)
	   (make-instance 'test-cairo-kids :fm-parent *parent*))))

(defmodel test-drawing (notebook)
  ()
  (:default-initargs
      :tab-labels '("Cairo")
    :kids (kids-list? (make-instance 'test-cairo-drawing :fm-parent *parent*))))


;;;
;;; GL drawing
;;;

(defparameter *da* nil)

(defmodel teapot (gl-drawing-area)
  ((teapot-size :accessor teapot-size :initarg :teapot-size))
  (:default-initargs
      :teapot-size (c? (widget-value :teapot-size))
      :width (c-in 200) :height (c-in 200) :expand t :fill t
      :init #'(lambda (self)
		(declare (ignorable self))
		(gl:clear-color 0 0 0 0)
		(gl:cull-face :back)
		(gl:depth-func :less)
		(gl:disable :dither)
		(gl:shade-model :smooth)
		(gl:light-model :light-model-local-viewer 1)
		(gl:color-material :front :ambient-and-diffuse)
		(gl:enable :light0 :lighting :cull-face :depth-test))
      :resize #'(lambda (self)
		  (with-matrix-mode (:projection)
		    (glu:perspective 50 (/ (allocated-width self) (allocated-height self)) 0.5 20)))
      :draw #'(lambda (self)
		(gl:load-identity)
		(gl:translate 0 0 -5)
		(gl:rotate 30 1 1 0)
		(gl:light :light0 :position '(0 1 1 0))
		(gl:light :light0 :diffuse '(0.2 0.4 0.6 0))
		(gl:clear :color-buffer-bit :depth-buffer-bit)
		(gl:color 1 1 1)
		(gl:front-face :cw)
		(trc "drawing teapot with size" (float (/ (teapot-size self) 100)))
		(glut:solid-teapot (/ (teapot-size self) 100))
		(gl:front-face :ccw)
		(gl:flush))))

(defobserver teapot-size ((self teapot))
  (redraw self))

(defmodel test-gl-drawing (gtk-app)
  ()
  (:default-initargs
      :kids (kids-list?
	     (make-kid 'hbox
		       :kids (kids-list?
			      (make-kid 'vbox
					:kids (kids-list?
					       (mk-spin-button :md-name :teapot-size
							       :min 1 :max 200 :step 1 :init 130)))
			      (make-kid 'teapot :md-name :teapot))))))


(defun test-drawing ()
  ;  (setf *da* (first (kids (first (kids (start-app 'test-gl-drawing))))))
  (start-app 'test-gl-drawing))
