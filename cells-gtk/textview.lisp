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

(def-gtk g-object text-buffer ()
  ((text :accessor text :initarg :text :initform nil)
   (tag-table :accessor tag-table :initarg :tag-table :initform (c-in nil))
   (markup :cell nil :accessor markup :initform nil))
  ()
  (modified-changed)
  :new-args (c_1 (list +c-null+)))

(defobserver text ((self text-buffer))
  (let ((buf (id self)))
    (gtk-text-buffer-set-text buf (or new-value "") -1)
    ;; Typically if you are interested in watching for modifications,
    ;; you don't want to see ones such as this, where it is done programmatically.
    ;; Thus we clear the modified flag. Your on-modified method can check for this.
    ;; It can even reset it, if you don't like this arrangement.
    (gtk-text-buffer-set-modified buf nil)))

(def-widget text-view (container)
  ((buffer :accessor buffer :initarg :buffer :initform (mk-text-buffer))
   (populate-popup :accessor populate-popup :initarg :populate-popup :initform (c-in nil))
   (depopulate-popup :accessor depopulate-popup :initarg :depopulate-popup :initform (c-in nil))
   (old-popups :cell nil :accessor old-popups :initform nil))
  (wrap-mode)
  ()
  :kids (c? (when (buffer self) (list (buffer self))))
  :expand t
  :fill t)

(defobserver buffer ((self text-view))
  (when new-value
    (gtk-text-view-set-buffer (id self) (id (buffer self)))))

;;; --------Populate-add -------------------------------------------------
;;; Menu-items that are appended to the existing textview popup menu on 
;;; the populate-popup signal. They are made fresh from populate-adds.

(defclass populate-adds ()
  ((label :initarg :label :initform nil)
   (on-activate :initarg :on-activate :initform nil)
   (owner :initarg :owner :initform nil)
   (kids :initarg :kids :initform nil)))

;;; Returns a list of populate-adds objects. These contain the :on-activate closures, 
;;; but do not create the menu-item, which must be made each time they are needed, 
;;; in the handler.
(defmacro def-populate-adds (&body menu-items)
  `(list 
    ,@(loop for (type . args) in menu-items 
           when (eql type :menu-item)
           collect `(funcall #'make-instance 'populate-adds ,@args))))

(cffi:defcallback text-view-populate-popup-handler :void
                  ((widget :pointer) (popup-menu :pointer) (data :pointer))
  (declare (ignore data signal))
  (when-bind (text-view (gtk-object-find widget))
    (when-bind (cb (callback-recover text-view :populate-popup))
      (funcall cb popup-menu)))
  1)

(defobserver populate-popup ((self text-view))
  (when new-value
      (callback-register self :populate-popup (populate-popup-closure (reverse new-value) self))
      (gtk-signal-connect (id self) "populate-popup"
                          (cffi:get-callback 'text-view-populate-popup-handler))))

(defun populate-popup-closure (p-adds text-view)
  (let (accum)
    (labels ((do-padds (p-add)
               (let ((item (with-slots (label on-activate owner kids) p-add
                             (mk-menu-item :label label :owner owner :on-activate on-activate
                                           :kids (mapcar #'do-padds kids)))))
                 (push item accum)
                 item)))
      #'(lambda (popup-menu)
          (loop for old in (old-popups text-view) do 
               (gtk-object-forget (id old) old))
          (let ((tops (mapcar #'do-padds p-adds)))
            (setf (old-popups text-view) accum)
            (mapc #'(lambda (i) (to-be i) (gtk-menu-shell-prepend popup-menu (id i))) tops))))))

(defvar *text-iters* nil "A RESLIST of gtk text-iters.")
(declaim (inline text-iters-reslist))
(defun text-iters-reslist ()
  (or *text-iters* 
      (setf *text-iters* (pod::new-reslist 100 :stock-fn #'gtk-adds-text-iter-new))))

(defmacro with-text-iters (vars &body body)
  "Create text iters, bind them to variables, execute and release iters."
  `(let (,@(loop for var in vars collect `(,var (pod::reslist-pop (text-iters-reslist)))))
     (unwind-protect 
       (progn ,@body)
       ,@(loop for var in vars collect `(pod::reslist-push ,var (text-iters-reslist))))))

(defun text-buffer-get-text (buffer &optional (start 0) stop)
  "Get text directly from buffer. Unlike the cell accessor, this sees edits."
  (let* ((buf (id buffer))
         (end (or stop (gtk-text-buffer-get-char-count buf))))
    (with-text-iters (s-iter e-iter)
      (gtk-text-buffer-get-iter-at-offset buf s-iter start)
      (gtk-text-buffer-get-iter-at-offset buf e-iter end)
      (gtk-text-buffer-get-text buf s-iter e-iter nil))))

(defun text-buffer-append-text (buffer text)
  "Append TEXT to end of BUFFER."
  (let ((buf (id buffer)))
    (with-text-iters (end)
      (gtk-text-buffer-get-end-iter buf end)
      (gtk-text-buffer-insert buf end text (length text))
      (gtk-text-buffer-set-modified buf t))))

(defun text-buffer-insert-text (buffer start text)
  (let ((buf (id buffer)))
    (with-text-iters (s-iter)
      (gtk-text-buffer-get-iter-at-offset buf s-iter start)
      (gtk-text-buffer-insert buf s-iter text (length text))
      (gtk-text-buffer-set-modified buf t))))

(defun text-buffer-delete-text (buffer start end)
  (let ((buf (id buffer)))
    (with-text-iters (s-iter e-iter)
      (gtk-text-buffer-get-iter-at-offset buf s-iter start)
      (gtk-text-buffer-get-iter-at-offset buf e-iter end)
      (gtk-text-buffer-delete buf s-iter e-iter))))

;;; This isn't working perfectly. Might be better to use gtk-text-view-scroll-mark-onscreen
;;; I do just that in one of my applications, but since it requires a mark, I didn't write
;;; an exported function for it. 
(defun text-view-scroll-to-position (textview pos)
  (let* ((view (id textview))
	 (buf (gtk-text-view-get-buffer view)))
  (with-text-iters (s-iter)
     (gtk-text-buffer-get-iter-at-offset buf s-iter pos)
     (gtk-text-view-scroll-to-iter view s-iter 0.0d0 nil 0.0d0 0.0d0))))

;;; The next two can be used to check and clear the the modified flag.
;;; The event is registered when you use :on-modified-changed on a text-buffer.
;;; The defobserver text ((text-buffer)) clears the flag so as to distinguish
;;; programmatic updates to the text from ones done by typing.
(defmethod text-buffer-modified-p ((buffer text-buffer))
  (gtk-text-buffer-get-modified (id buffer)))

(defmethod (setf text-buffer-modified-p) (val (buffer text-buffer))
  (gtk-text-buffer-set-modified (id buffer) val))
