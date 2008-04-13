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
;;; the dialog
;;;

(defmodel test-cairo-drawing (vbox)
  ((new-prim :accessor new-prim :initform (c-in nil)))
  (:default-initargs
      :md-name :test-drawing
    :kids (kids-list?
	   (mk-hbox :fill t :expand t
		    :kids (kids-list?
			   (make-instance 'cairo-drawing-area :md-name :draw :expand t :fill t
					  :width 500 :height 500)
			   (mk-vbox
			    :kids (kids-list?
				   (mk-button :label "Draw Box"
					      :on-clicked (callback (w e d)
							    (let* ((p1 (random-point 10 10 480 480))
								   (p2 (2d:v+ p1 (random-point 10 10 40 40)))
								   (col1 (random-color))
								   (col2 (random-color)))
							      (trcx "rect" p1 p2 col1 col2)
							      (mk-primitive (fm-other :draw) :rectangle
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
							      (mk-primitive (fm-other :draw) :arc
									    :p (c-in p)
									    :radius (c-in radius)
									    :rgb (rgb? col1)
									    :fill-rgb (rgb? col2)
									    :alpha (alpha? 1)
									    :filled t
									    :draggable t
									    :selectable t))))
				   )))))))

;;;
;;; a test-drawing tab
;;; 

(defmodel test-drawing (notebook)
  ()
  (:default-initargs
      :tab-labels (list "Cairo")
    :kids (kids-list?
	   (make-instance 'test-cairo-drawing))))

(defparameter *da* nil)

(defun test-cairo-drawing ()
  (setf *da* (first (kids (first (kids (start-win 'test-cairo-drawing)))))))


