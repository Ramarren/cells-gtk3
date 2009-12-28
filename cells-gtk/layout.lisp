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

(def-widget box (container)
  ()
  (homogeneous spacing)
  ()
  :value (c-in nil)
  :homogeneous (c-in nil)
  :spacing (c-in 0))

(defobserver .kids ((self box))
  (when new-value
    (dolist (kid new-value)
        (gtk-box-pack-start (id self) (id kid) 
                            (expand? kid) (fill? kid) (padding? kid)))))

(def-widget hbox (box)
  () () ()
  :new-args (c_1 (list (homogeneous self) (spacing self))))
(def-widget vbox (box)
  () () ()
  :new-args (c_1 (list (homogeneous self) (spacing self))))

(def-widget table (container)
  ((elements :accessor elements :initarg :elements :initform (c-in nil))
   (homogeneous :accessor homogeneous :initarg :homogeneous :initform nil)
   (rows-count :accessor rows-count :initarg :rows-count :initform (c? (length (elements self))))
   (cols-count :accessor cols-count :initarg :cols-count
               :initform (c? (let ((elems (elements self)))
                               (if elems (apply #'max (mapcar #'length elems)) 0)))))
  ()
  ()
  :new-args (c_1 (list (rows-count self) (cols-count self) (homogeneous self)))
  :kids (c? (apply #'append (mapcar (lambda (x) (remove-if #'null x)) 
				    (elements self)))))

(defun next-row-item-not-null (row start-col)
  (or 
   (loop for item in (subseq row (1+ start-col))
      for pos from (1+ start-col) do
	(when item (return pos))
	finally (return pos))
   #+not (1+ start-col) ; ph: sbcl says this is unreachable (and is quite right)
   ))

(defobserver elements ((self table))
  (loop for row in new-value
        for row-num from 0 do
       (loop for kid in row
	     for col-num from 0 do
	    (when kid
	      (gtk-table-attach (id self) (id kid) 
				col-num (next-row-item-not-null row col-num)
				row-num (1+ row-num)
				(logior (if (x-expand kid) (ash 1 0) 0) (if (x-fill kid) (ash 1 2) 0))
				(logior (if (y-expand kid) (ash 1 0) 0) (if (y-fill kid) (ash 1 2) 0))
				(x-pad kid)
				(y-pad kid))))))

(def-widget hpaned ()
  ((divider-pos :accessor divider-pos :initarg :divider-pos :initform (c-in 0)))
  ()
  ())

(defobserver divider-pos ((self hpaned))
  (when new-value
    (gtk-paned-set-position (id self) new-value)))

(defobserver .kids ((self hpaned))
  (when new-value
    (gtk-paned-add1 (id self) (id (make-be 'frame 
						    :shadow 'in
						    :kids (kids-list? (first new-value)))))
    (and (cadr new-value)
	 (gtk-paned-add2 (id self) (id (make-be 'frame 
						    :shadow 'in
						    :kids (kids-list? (cadr new-value))))))))

(def-widget vpaned ()
  ((divider-pos :accessor divider-pos :initarg :divider-pos :initform (c-in 0)))
  ()
  ())

(defobserver divider-pos ((self vpaned))
  (when new-value
    (gtk-paned-set-position (id self) new-value)))

(defobserver .kids ((self vpaned))
  (when new-value
    (gtk-paned-add1 (id self) (id (make-be 'frame 
						    :shadow 'in
						    :kids (kids-list? (first new-value)))))
    (and (cadr new-value)
	 (gtk-paned-add2 (id self) (id (make-be 'frame 
						    :shadow 'in
						    :kids (kids-list? (cadr new-value))))))))

(def-widget frame (container)
  ((shadow :accessor shadow? :initarg :shadow :initform nil)
   (markup :accessor markup :initarg :markup :initform nil)
   (markup-label-widget :accessor markup-label-widget :owning t :initarg :markup-label-widget)
   (label :accessor label :initarg :label :initform (c-in nil)))
  (label-widget label-align shadow-type)
  ()
  :shadow-type (c-in nil)
  :new-args (c_1 (list nil))
  :markup-label-widget (c? (when (markup self)
                             (mk-label :markup (markup self)))))

(defobserver label ((self frame))
  (when new-value
    (gtk-frame-set-label (id self) new-value)))

(defobserver markup ((self frame))
  (when new-value
    (with-integrity (:change 'frame-markup)
      (setf (label-widget self) (id (markup-label-widget self))))))

(defobserver shadow ((self frame))
  (when new-value
    (with-integrity (:change 'frame-shadpw)
      (setf (shadow-type self)
        (ecase new-value
          (none 0)
          (in 1)
          (out 2)
          (etched-in 3)
          (etched-out 4))))))

(defobserver .kids ((self frame))
  (assert-bin self)
  (dolist (kid new-value)
    (gtk-container-add (id self) (id kid))))

(def-widget aspect-frame (frame)
  ((xalign :accessor xalign :initarg :xalign :initform 0.5)
   (yalign :accessor yalign :initarg :yalign :initform 0.5)
   (ratio :accessor ratio? :initarg :ratio :initform 1)
   (obey-child :accessor obey-child :initarg :obey-child :initform nil)) 
  () ()
  :new-args (c_1 (list 
		 nil
		 (coerce (xalign self) 'single-float)
		 (coerce (yalign self) 'single-float)
		 (coerce (ratio? self) 'single-float)
		 (obey-child self))))

(def-widget hseparator ()
  () () ())

(def-widget vseparator ()
  () () ())

(def-widget expander (container)
  ((label :accessor label :initarg :label :initform (c-in nil)))
  (expanded spacing use-underline use-markup label-widget)
  ()
  :new-args (c_1 (list nil)))

(defobserver label ((self expander))
  (when new-value
    (gtk-expander-set-label (id self) new-value)))

(defobserver .kids ((self expander))
  (assert-bin self)
  (dolist (kid new-value)
    (gtk-container-add (id self) (id kid))))

(def-widget scrolled-window (container)
  ()
  (policy placement shadow-type)
  ()
  :expand t :fill t
  :policy (list 1 1)
  :new-args (list +c-null+ +c-null+))

(defobserver .kids ((self scrolled-window))
  (assert-bin self)
  (dolist (kid new-value)
    (if (member (class-name (class-of kid)) '(listbox treebox tree-view text-view layout) :test #'equal)
	(gtk-container-add (id self) (id kid))
	(gtk-scrolled-window-add-with-viewport (id self) (id kid)))))

(def-widget notebook (container)
  ((tab-labels :accessor tab-labels :initarg :tab-labels :initform (c-in nil))
   (tab-labels-widgets :accessor tab-labels-widgets :initform (c-in nil))
   (show-page :accessor show-page :initarg :show-page :initform (c-in 0))
   (tab-pos :accessor tab-pos :initarg :tab-pos :initform (c-in nil))
   (selected-page :accessor selected-page :initform (c-in nil)))
  (current-page show-tabs show-border scrollable tab-border 
   homogeneous-tabs)
  (select-page)
  :current-page (c-in nil)
  :show-tabs (c-in t)
  :on-select-page (callback (w e d)
		    (with-integrity (:change :selected-page)
		      (trc "on select page is called" self (when self (kids self)))
		      (when (and self (kids self))
		       (setf (selected-page self)
			     (nth (gtk-notebook-get-current-page (id self)) (kids self)))))))

(defobserver tab-pos ((self notebook))
  (when new-value
    (gtk-notebook-set-tab-pos 
     (id self) 
     (case new-value
       (:left 0)
       (:right 1)
       (:top 2)
       (:bottom 3)
       (t 2)))))

(defun notebook-contains-page-p (notebook widget &aux (wid (cffi:pointer-address (id widget))))
  (loop for i from 1 to (gtk-notebook-get-n-pages (id notebook))
	for page = (gtk-notebook-get-nth-page (id notebook) (1- i))
	when (= wid (cffi:pointer-address page)) return t))

(defobserver show-page ((self notebook))
  (when (and new-value (>= new-value 0) (< new-value (length (kids self))))
    (setf (current-page self) new-value)))
    
(defobserver .kids ((self notebook))
					;(dolist (widget (tab-labels-widgets self)) ;; This was from the original code. 
					;      (not-to-be widget))                  ;; It causes errors.
  (with-integrity (:change 'notebook-kids)
    (loop for kid in new-value
       for pos from 0
       for label = (nth pos (tab-labels self)) 
       unless (notebook-contains-page-p self kid) do
       (let ((lbl (and label (make-be 'label :text label))))
	 (when lbl (push lbl (tab-labels-widgets self)))
	 (gtk-notebook-append-page (id self) (id kid) (and lbl (id lbl)))))
    (loop for page from 0 to (length new-value) do
	 (setf (current-page self) page)) 
    (when (and (show-page self) (>= (show-page self) 0) (< (show-page self) (length new-value)))
      (setf (current-page self) (show-page self)))))

(defobserver show-tabs ((self notebook))
 (gtk-notebook-set-show-tabs (id self) new-value))

(defobserver current-page ((self notebook))
 (when new-value (gtk-notebook-set-current-page (id self) new-value)))

(defobserver show-border ((self notebook))
 (gtk-notebook-set-show-border (id self) new-value))

(def-widget alignment (container)
  ((xalign :accessor xalign :initarg :xalign :initform 0.5)
   (yalign :accessor yalign :initarg :yalign :initform 0.5)
   (xscale :accessor xscale :initarg :xscale :initform 0)
   (yscale :accessor yscale :initarg :yscale :initform 0))
  ()
  ()
  :new-args (c_1 (list (coerce (xalign self) 'single-float)
		      (coerce (yalign self) 'single-float)
		      (coerce (xscale self) 'single-float)
		      (coerce (yscale self) 'single-float))))

(defobserver xalign ((self alignment))
  (when new-value
    (gtk-alignment-set 
     (id self)
     (coerce (xalign self) 'single-float)
     (coerce (yalign self) 'single-float)
     (coerce (xscale self) 'single-float)
     (coerce (yscale self) 'single-float))))
(defobserver yalign ((self alignment))
  (when new-value
    (gtk-alignment-set 
     (id self)
     (coerce (xalign self) 'single-float)
     (coerce (yalign self) 'single-float)
     (coerce (xscale self) 'single-float)
     (coerce (yscale self) 'single-float))))
(defobserver xscale ((self alignment))
  (when new-value
    (gtk-alignment-set 
     (id self)
     (coerce (xalign self) 'single-float)
     (coerce (yalign self) 'single-float)
     (coerce (xscale self) 'single-float)
     (coerce (yscale self) 'single-float))))
(defobserver yscale ((self alignment))
  (when new-value
    (gtk-alignment-set 
     (id self)
     (coerce (xalign self) 'single-float)
     (coerce (yalign self) 'single-float)
     (coerce (xscale self) 'single-float)
     (coerce (yscale self) 'single-float))))
				

(defobserver .kids ((self alignment))
  (assert-bin self)
  (dolist (kid new-value)
    (gtk-container-add (id self) (id kid))))
