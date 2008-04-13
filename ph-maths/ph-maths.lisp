
;;;
;;; Linear algebra 2d

(defpackage :ph-maths-2d
  (:use :cl)
  (:nicknames :2d)
  (:export :v
	   :x
	   :y
	   :v+
	   :v-
	   :v*
	   :min-abs
	   :max-abs
	   :v-polar
	   :r
	   :phi
	   :polar-coords
	   :polar-radius
	   :polar-angle
	   :cartesian-coords
	   :abs-angle
	   :deg->rad
	   :rad->deg
	   :v0
	   :vp
	   :p
	   :<>
	   :intersect-line-circle
	   :distance-point-line
	   :point-in-box-p
	   :to-decimal
	   :to-rgb
	   :to-rgb-vector
	   :~=))

(in-package :ph-maths-2d)

(declaim (optimize (speed 1) (debug 3) (space 0)))


(defun denil (lst)
  (loop for val in lst if val collect val))

;;; represent 2d vector as cons

(declaim (inline v x y))

(defun v (x y)
  (cons x y))

(defun x (v)
  (car v))

(defun y (v)
  (cdr v))

;;; basic linear algebra

(declaim (inline v-reduce v+ v- v*))

(defun v-reduce (fn vectors)
  (declare (function fn))
  (reduce #'(lambda (v1 v2) (v (funcall fn (x v1) (x v2)) (funcall fn (y v1) (y v2)))) (denil vectors)))

(defun v+ (&rest vectors)
  (v-reduce #'+ vectors))

(defun v- (&rest vectors)
  (v-reduce #'- vectors))

(defun v* (lambda vector)
  (v (* (x vector) lambda) (* (y vector) lambda)))

;;; min/max

(declaim (inline abs-reduce min-abs max-abs))

(defun abs-reduce (fn vals)
  (reduce fn (denil vals) :key #'abs))

(defun min-abs (&rest vals)
  (abs-reduce #'min vals))

(defun max-abs (&rest vals)
  (abs-reduce #'max vals))

; polar coordinates

(declaim (inline v-polar phi r polar-radius polar-angle polar-coords))

(defun v-polar (phi r)
  (cons phi r))

(defun phi (v)
  (car v))

(defun r (v)
  (cdr v))


(defun polar-radius (v)
  "return radius of cartesian vector v"
  (sqrt (+ (* (x v) (x v)) (* (y v) (y v)))))

(defun polar-angle (v)
  "return angle of cartesian vector v"
  (if (zerop (x v))
      (if (>= (y v) 0)
          #.(* pi -0.5)
          #.(* pi 0.5))
      (atan (- (y v)) (x v))))

(defun polar-coords (v)
  "return a polar representation of cartesian vector v"
  (v-polar (polar-angle v) (polar-radius v)))

; cartesian coords

(declaim (inline cartesian-coords))

(defun cartesian-coords (v-polar)
  "returns a cartesian representation of polar vector v-polar"
  (v (* (r v-polar) (cos (phi v-polar)))
     (* -1 (r v-polar) (sin (phi v-polar)))))

; degrees

(declaim (inline deg->rad rad->deg abs-angle))

(defun deg->rad (degs)
  (/ (* degs pi) 180.0))

(defun rad->deg (rads)
  (* (/ rads pi) 180.0))

(defun abs-angle (phi)
  "returns a positive angle 0 <= phi <= 2pi"
  (cond
    ((or (= phi #.(* 2 pi)) (= phi #.(* -2 pi))) phi)
    (t (mod phi #.(* 2 pi)))))


  
; albegra -- 2d

(declaim (inline v0 vp <> p))

(defun v0 (v)
  "returns a vector with the same direction as v and unit length"
  (let ((r (polar-radius v)))
    (if (plusp r) (v* (/ 1 r) v) (v 0 0))))

(defun vp (v)
  "returns a unit vector perpendicular to v"
  (let ((u (v0 v)))
    (v (- (y u)) (x u))))

(defun <> (v1 v2)
  "returns the scalar product <v1, v2>"
  (+ (* (x v1) (x v2)) (* (y v1) (y v2))))

(defun p (v1 v2)
  "returns the projection of v1 onto v2.  Second return value is the length of the projection."
  (let* ((v2_0 (v0 v2))
         (len (<> v2_0 v1)))
    (values (v* len v2_0) len)))

(declaim (inline distance-point-line intersect-line-circle point-in-box-p))

(defun distance-point-line (point p1 p2)
  "returns the shortest distance from point to the line p1,p2."
  (abs (second (multiple-value-list (p (v- p1 point) (vp (v- p2 p1)))))))


(defun intersect-line-circle (p1 p2 r)
  "returns the intersection of a line through p1 and p2 and a circle around p2 with radius r"
  (v+ p2 (v* r (v0 (v- p1 p2)))))

    
(defun point-in-box-p (p p1 p2 &key (tol 0))
  "returns true if p is inside the box given by p1,p2"
  (and (< (- (min (x p1) (x p2)) tol) (x p) (+ (max (x p1) (x p2)) tol))
       (< (- (min (y p1) (y p2)) tol) (y p) (+ (max (y p1) (y p2)) tol))))



;; base conversion

(defun to-decimal (val &key (base 16))
  "converts val (a value in base base as a string) to an integer"
  (loop for p from 0 to (1- (length val))
					for x downfrom (1- (length val))
					summing (* (let ((c (char-code (char val p))))
						     (cond ((< 47 c 58) (- c 48))
							   ((< 64 c 91) (- c 55)) 
							   ((< 96 c 123) (- c 87))
							   (t (warn "Illegal character in hex argument to to-decimal") 0)))
						   (expt base x))))

(defun to-rgb (html-color)
  "parses an html color code like #A204B2 to '(.8 .01 .7 4)"
  (loop for val from 0 to 2
        for pos = (1+ (* val 2))
        collecting (/ (to-decimal (subseq html-color pos (+ pos 2))) 256)))

(defun to-rgb-vector (html-color)
  "parses an html color code like #A204B2 to #(.8 .01 .7 4)"
  (coerce (loop for val from 0 to 2
                for pos = (1+ (* val 2))
                collecting (/ (to-decimal (subseq html-color pos (+ pos 2))) 256))
          'vector))




   
; fuzzy comparison

(defun ~= (&rest params)
  (if (cdr params)
      (let ((max (apply #'max params))
            (min (apply #'min params)))
        (> .05 (abs (/ (- max min) (max (abs max) (abs min) 1d-8)))))
      t))
