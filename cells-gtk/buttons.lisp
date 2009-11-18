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

(def-widget button (container)
  ((stock :accessor stock :initarg :stock :initform (c-in nil))
   (markup :accessor markup :initarg :markup :initform nil)
   (label :accessor label :initarg :label :initform (c-in nil))
   (clicked :accessor clicked :initform (c-in nil) :cell :ephemeral))
  (relief use-stock use-underline)
  (activate clicked enter leave pressed released)
  :kids (c-in nil)
  :on-clicked (callback (widget event data)
                (when *gtk-debug*
                  (trc "button clicked" widget))
                (with-integrity (:change 'button-clicked-cb)
                  (setf (clicked self) t))))

(defobserver label ((self button))
  (when new-value
    (gtk-button-set-label (id self) new-value)))

(defobserver markup ((self button))
  (when new-value    
    (with-integrity (:change 'button-markup)
      (setf (kids self) (the-kids (mk-label :markup new-value))))))

(defobserver .kids ((self button))
  (assert-bin self)
  (dolist (kid (kids self))
    (gtk-container-add (id self) (id kid))))

(defobserver stock ((self button))
  (when new-value
    (with-integrity (:change 'button-stock-observer)
      (setf (label self) (string-downcase (format nil "gtk-~a" new-value)))
      (trc nil "c-outputting stock" (label self)) (force-output)
      (setf (use-stock self) t))))

(def-widget toggle-button (button)
  ((init :accessor init :initarg :init :initform nil))
  (mode active)
  (toggled)
  :active (c-in nil)
  :on-toggled (callback (widget event data)
                (when *gtk-debug*
                  (trc "toggle-button toggled" widget))
                (with-integrity (:change 'tggle-button-on-toggled-cb)
                  (let ((state (gtk-toggle-button-get-active widget)))
                    (setf (value self) state)))))

#+test
(DEF-GTK WIDGET TOGGLE-BUTTON (BUTTON) ((INIT :ACCESSOR INIT :INITARG :INIT :INITFORM NIL))
         (MODE ACTIVE) (TOGGLED) :ACTIVE (C-IN NIL) :ON-TOGGLED
         (CALLBACK (WIDGET EVENT DATA)
                   (LET ((STATE (GTK-TOGGLE-BUTTON-GET-ACTIVE WIDGET)))
                     (SETF (value SELF) STATE))))

#+test
(defobserver ACTIVE ((SELF TOGGLE-BUTTON))
                     (WHEN (OR NEW-VALUE OLD-VALUE)
                       (CONFIGURE SELF #'GTK-TOGGLE-BUTTON-SET-ACTIVE NEW-VALUE)))

(defobserver init ((self toggle-button))
  (setf (active self) new-value)
  (setf (value self) new-value))

(def-widget check-button (toggle-button)
  () () ())

(def-widget radio-button (check-button)
  () () ()
  :new-tail (c_1 (and (upper self box)
		     (not (eql (first (kids (fm-parent self))) self))
		     '-from-widget))
			 
  :new-args (c_1 (assert (upper self box))
              (and (upper self box)
                (list
                 (if (eql (first (kids .parent)) self) 
                     +c-null+
                   (id (first (kids .parent)))))))
  :on-toggled  (callback (widget event data)
                 (with-integrity (:change 'radio-butt-on-tog)
                   (let ((state (gtk-toggle-button-get-active widget)))
                     (setf (value self) state)))))
  
(defobserver .value ((self radio-button))
  (when (and new-value (upper self box))
    (with-integrity (:change 'radio-up-to-box)
      (setf (value (upper self box)) (md-name self)))))
