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

(defun to-str (sym)
  (if (stringp sym)
      sym
      (string-downcase (format nil "~a" sym))))

(defmacro with-markup (markup &rest rest)
  (destructuring-bind (&key font-desc font-family face size style
			    weight variant stretch foreground background
			    underline rise strikethrough fallback lang) markup
    (let ((markup-start
	   `(format nil "<span~{ ~a=~s~}>"
	     (list 
	      ,@(when font-desc `("font_desc" (to-str ,font-desc)))
	      ,@(when font-family `("font_family" (to-str ,font-family)))
	      ,@(when face `("face" (to-str ,face)))
	      ,@(when size `("size" (to-str ,size)))
	      ,@(when style `("style" (to-str ,style)))
	      ,@(when weight `("weight" (to-str ,weight)))
	      ,@(when variant `("variant" (to-str ,variant)))
	      ,@(when stretch `("stretch" (to-str ,stretch)))
	      ,@(when foreground `("foreground" (to-str ,foreground)))
	      ,@(when background `("background" (to-str ,background)))
	      ,@(when underline `("underline" (to-str ,underline)))
	      ,@(when rise `("rise" (to-str ,rise)))
	      ,@(when strikethrough `("strikethrough" (if ,strikethrough "true" "false")))
	      ,@(when fallback `("fallback" (to-str ,fallback)))
	      ,@(when lang `("lang" (to-str ,lang)))))))
      
      `(format nil "~a ~a </span>" ,markup-start (format nil "~{~a~}" (list ,@rest))))))
	    

;;;
;;; misc
;;;

;;; adds padding and alignment to label, arrow, image, and (pixmap)

(defmd misc ()
  xalign :xalign (c-in .5)
  yalign :yalign (c-in .5)
  xpad :xpad (c-in 0)
  ypad :ypad (c-in 0))

(defobserver xalign ((self misc))
  (gtk-misc-set-alignment (id self) (^xalign) (^yalign)))

(defobserver yalign ((self misc))
  (gtk-misc-set-alignment (id self) (^xalign) (^yalign)))

(defobserver xpad ((self misc))
  (gtk-misc-set-padding (id self) (^xpad) (^ypad)))

(defobserver ypad ((self misc))
  (gtk-misc-set-padding (id self) (^xpad) (^ypad)))

;;;
;;; label
;;; 

(def-widget label (widget misc)
  ((markup :accessor markup :initarg :markup :initform nil)
   (text :accessor text :initarg :text :initform nil))
  (line-wrap selectable use-markup)
  ()
  :text (c-in nil)
  :use-markup (c? (not (null (markup self))))
  :new-args (c_1 (list nil))
  :xalign (c-in 0.0))

(defobserver text ((self label))
  (when new-value
    (gtk-label-set-text-with-mnemonic (id self) new-value)))

(defobserver markup ((self label))
  (when new-value
    (gtk-label-set-markup-with-mnemonic  (id self) new-value)))

(def-widget accel-label ()
  ((text :accessor text :initarg :text :initform nil))
  ()
  ()
  :new-args (c? (list (text self))))

(def-widget image (widget misc)
  ((filename :accessor filename :initarg :filename :initform nil)
   (stock :accessor stock :initarg :stock :initform nil)
   (stock-id :accessor stock-id
	     :initform (c? (format nil "gtk-~a" (string-downcase (symbol-name (stock self))))))
   (icon-size :accessor icon-size :initarg :icon-size :initform nil)
   (icon-size-id :accessor icon-size-id
		 :initform (c? (case (icon-size self)
				 (:menu 1)
				 (:small-toolbar 2)
				 (:large-toolbar 3)
				 (:button 4)
				 (:dnd 5)
				 (:dialog 6)
				 (t 4)))))
  ()
  ()
  :new-tail (c? (if (stock self)
		    '-from-stock
		    '-from-file))
  :new-args (c_1 (if (stock self)
		    (list (stock-id self) (icon-size-id self))
		    (list (or (filename self) "")))))

(defobserver stock ((self image))
  "Set the image of a stock icon"
  (when new-value
    (gtk-image-set-from-stock (id self) (stock-id self) (icon-size-id self))))

(def-widget statusbar ()
  ((contexts :accessor contexts :initform (make-hash-table)))
  (has-resize-grip)
  ()
  :has-resize-grip t)

(defmethod new-context ((self statusbar) context)
  (setf (gethash context (contexts self))
    (gtk-statusbar-get-context-id (id self) (format nil "~a" context))))

(defmethod push-message ((self statusbar) message &optional (context 'main))
  (let ((id (gethash context (contexts self))))
    (when id
      (gtk-statusbar-push (id self) id message))))

(defmethod pop-message ((self statusbar) &optional (context 'main))
  (let ((id (gethash context (contexts self))))
    (when id
      (gtk-statusbar-pop (id self) id))))

(defmethod initialize-instance :after ((self statusbar) &rest rest)
  (new-context self 'main))

(def-widget progress-bar ()
  ((text :accessor text :initarg :text :initform (c-in nil))
   (fraction :accessor fraction :initarg :fraction :initform (c-in nil))
   (pulse-step :accessor pulse-step :initarg :pulse-step :initform (c-in nil))
   (orientation :accessor orientation :initarg :orientation :initform (c-in nil)))
  ()
  ())

(defmethod pulse ((self progress-bar))
  (gtk-progress-bar-pulse (id self)))

(defobserver fraction ((self progress-bar))
  (when new-value
    (gtk-progress-bar-set-fraction (id self) (coerce new-value 'double-float))))

(defobserver pulse-step ((self progress-bar))
  (when new-value
    (gtk-progress-bar-set-pulse-step (id self) (coerce new-value 'double-float))))

(defobserver orientation ((self progress-bar))
  (when new-value
    (gtk-progress-bar-set-orientation (id self)
      (case new-value
	(:right 0)
	(:left 1)
	(:top 2)
	(:bottom 3)
	(t 0)))))

