
#|

Macros to make threading a little less implemetation dependent.

(w) Peter Hildebrandt, 01/2008

|#

(in-package :gtk-ffi)
;;;
;;; -------- Make GDK Threadsafe ---------------------------------------------
;;;

;;; the gdk-thread lock is not recursive
;;; here we simulate that behavior
  

(defvar *gdk-lock-owner* nil "The current owner of the lock")
(defvar *gdk-lock-main-thread* nil "The gtk-main thread (needs no locks)")

(defun gdk-threads-register-gtk-main (gtk-main-thread)
  "register gtk-main-thread.  Locking attempts from this thread will be ignored."
  (setf *gdk-lock-main-thread* gtk-main-thread))

(defmacro with-gdk-threads (&rest body)
  #-cells-gtk-threads
  `(unwind-protect (progn
		     (gdk-threads-enter)
		     ,@body)
     (gdk-threads-leave))
  #+cells-gtk-threads
  (with-gensyms (bodyfn thrd)
    `(flet ((,bodyfn () ,@body))
       (let ((,thrd (bt:current-thread)))
	 (cond
	   ((eq ,thrd *gdk-lock-main-thread*) (,bodyfn)) ; free pass for the main thread, since it will have a lock -- trust gtk
	   ((eq ,thrd *gdk-lock-owner*) (,bodyfn)) ; recursive attempt -- we already have the lock
	   (t (unwind-protect ; behave as usual, but make sure subsequent attempts from the same thread are passed through
		   (progn
		     #+msg (format t "~&GDK-LOCK REQUEST ~a ~a (current owner ~a)" ,thrd ',body *gdk-lock-owner*)
		     (gdk-threads-enter)
		     #+msg (format t "--> GRANTED")
		     (setf *gdk-lock-owner* ,thrd)
		     (,bodyfn))
		(setf *gdk-lock-owner* nil)
		#+msg (format t "--> RELEASE~%")
		(gdk-threads-leave))))))))



