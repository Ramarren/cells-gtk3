(in-package :test-gtk)

;;; The details of the tag-table and markup slots are, for the time being, 
;;; not defined by cells-gtk. The demo give an idea how they might be used.

(defmodel test-textview (vbox)
  ((buffer :accessor buffer :initarg :buffer 
	   :initform (mk-text-buffer 
		      :text (format nil "~{~a~%~}" 
                                    (loop for i below 100 collect (format nil "Text view Line ~a" i)))
                      :on-modified-changed 
                      (callback (w e d)
                        (show-message "Text buffer modified"))
                      :tag-table (c? (xtv-create-tag-table self)))))
  (:default-initargs
      :kids (kids-list?
	     (mk-scrolled-window 
	      :kids (kids-list?
		     (mk-text-view 	      
		      :buffer (c? (buffer (upper self test-textview)))
                      :populate-popup
                      (c?
                        (def-populate-adds
                          (:menu-item :label "Mark something yellow"
                                      :owner self
                                      :on-activate 
                                      (callback (w e d)
                                        (let ((buf (buffer (owner self))))
                                          (setf (markup buf) ; implementation idea... ;^)
                                                (list 
                                                 (make-instance 'color-tag :start 10 :end 20
                                                                :name :yellow-background)))
                                          (apply-markup (first (markup buf)) buf))))))))))))
                                         
(defmethod xtv-create-tag-table ((self text-buffer))
  (let ((ht (make-hash-table)))
    (flet ((create-tag (name)
             (setf (gethash name ht)
                     (gtk-text-buffer-create-tag 
                      (cgtk::id self)
                      (string-downcase (symbol-name (gensym)))
                      (string-downcase (subseq (string name) (1+ (position #\- (string name)))))
                      (string-downcase (subseq (string name) 0 (position #\- (string name))))
                      +c-null+))))
      (loop for name in '(:red-foreground :red-background :yellow-foreground :yellow-background)
         do (create-tag name)))
    ht))

;;; In a real application you might use an xml parser on marked up text 
;;; by creating 'mark objects' like these. In the demo we just do it in a 
;;; menu item cb.  

;;; We won't even use this one here, but marks are useful ;^)
(defclass mark ()
  ((type :initarg :type)
   (pos :initarg :pos)
   (len :initarg :len)
   (c-ptr :accessor c-ptr :initform nil)))

(defclass color-tag ()
  ((name :initarg :name)
   (start :initarg :start)
   (end :initarg :end)))

(defmethod apply-markup ((tag color-tag) buffer)
  (with-slots (start end name) tag
    (assert (gethash name (tag-table buffer)))
    (let ((buf (cgtk::id buffer)))
      (with-text-iters (start-iter end-iter)
        (gtk-text-buffer-get-iter-at-offset buf start-iter start)
        (gtk-text-buffer-get-iter-at-offset buf end-iter end)
        (gtk-text-buffer-apply-tag buf (gethash name (tag-table buffer)) start-iter end-iter)))))

(defmethod apply-markup ((mark mark) buffer)
  (with-slots (pos c-ptr) mark
    (let ((buf (cgtk::id buffer)))
      (with-text-iters (iter)
        (gtk-text-buffer-get-iter-at-offset buf iter pos)
        (setf c-ptr (gtk-text-buffer-create-mark buf (symbol-name (gensym)) iter t))))))
