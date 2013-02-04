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

;;;
;;; Debugging tool
;;;

(declaim (optimize (debug 3) (speed 0)))

(defmacro with-trc (form)
  (with-gensyms (res)
    `(progn
       (when *gtk-debug* (format t "~&eval ~a " ',form))
       (let ((,res ,form))
	 (when *gtk-debug*
	   (format t "--> ~a~%" ,res)
	   (force-output))
	 ,res))))

(defmacro with-trcs (&body body)
  `(progn
      ,@(mapcar #'(lambda (f) `(with-trc ,f)) body)))

;;;
;;; Main application class
;;; 

(defmodel gtk-app (window)
  ((splash-screen-image :accessor splash-screen-image :initarg :splash-screen-image :initform nil)
   (tooltips :initarg :tooltips :accessor tooltips :owning t :initform (make-be 'tooltips))
   (tooltips-enable :accessor tooltips-enable :initarg :tooltips-enable :initform (c-in t))
   (tooltips-delay :accessor tooltips-delay :initarg :tooltips-delay :initform (c-in nil))
   (stock-icons :cell nil :accessor stock-icons :initarg :stock-icons :initform nil)
   (rc-file :cell nil :initarg :rc-file :initform nil)
   (terminate-on-close :accessor terminate-on-close :cell nil :initform nil :initarg :terminate-on-close))
  (:default-initargs
   :on-delete-event (lambda (self widget event data)
		      (declare (ignore widget event data))
		      (not-to-be self)
		      (when (terminate-on-close self)		      
			(with-trc (gtk-main-quit)))
		      0)))

(defmethod initialize-instance :after ((self gtk-app) &key stock-icons)
  (loop for (name pathname) in stock-icons do
       (let* ((image (gtk-image-new-from-file pathname))
              (pixbuf (gtk-image-get-pixbuf image))
              (icon-set (gtk-icon-set-new-from-pixbuf pixbuf))
              (factory (gtk-icon-factory-new)))
         (gtk-icon-factory-add factory (format nil "gtk-~A" (string-downcase (string name))) icon-set)
         (gtk-icon-factory-add-default factory))))


(defobserver tooltips-enable ((self gtk-app))
  (when (tooltips self)
    (if new-value
        (gtk-tooltips-enable (id (tooltips self)))
      (gtk-tooltips-disable (id (tooltips self))))))

(defobserver tooltips-delay ((self gtk-app))
  (when new-value
      (gtk-tooltips-set-delay (id (tooltips self)) new-value)))
      
(defmodel splash-screen (window)
  ((image-path :accessor image-path :initarg :image-path :initform nil))
  (:default-initargs
      :decorated nil
      :position :center
      :kids (c? (when (image-path self)
		  (the-kids
		   (mk-image :filename (image-path self)))))))


;;;
;;; Bookkeeping
;;;

;;; We keep track of all windows in the list *system*

(defvar *system* nil "keeps track of all running windows")

(defun open-windows ()
  "Return a list of open windows"
  (remove-if-not #'(lambda (w) (gtk-object-find (id w))) *system*))


;;;
;;; Simulating an application main window
;;; 

;;; We use the gtk-main-quit callback to close open application windows when the main
;;; window is closed (in case you have defined one by setting :terminate-on-close t
;;; for gtk-app


(cffi:defcallback cb-quit :unsigned-int ((data :pointer))
  (trc "cb quit" data (gtk-object-find data))
  (bwhen (self (gtk-object-find data))
    (setf *system* (delete self *system*))
    (not-to-be self))
  0)

(defmethod initialize-instance :after ((self window) &rest initargs)
  (declare (ignore initargs))
  (push self *system*)
  ;; (trcx "initialize-instance :after" self (id self))
  (when (id self)
   (setf (slot-value self 'cb-quit-id) (gtk-quit-add 0 (cffi:get-callback 'cb-quit) (id self)))))

(defmethod not-to-be :after ((self window))
  (with-trc (gtk-quit-remove (slot-value self 'cb-quit-id))))

;;;
;;; Helper functions convering the life cycle of an application
;;; 

(defvar *using-thread* 'undecided)

;;; Initialize GDK

(defvar *gtk-loaded* #+clisp t #-clisp nil) ;; kt: looks like CLisp does this on its own
(defvar *gtk-initialized* nil)

(defun cells-gtk-init ()
  "initialize cells-gtk.  DO NOT USE WITH THREADING"
  #-cmu
  (unless *gtk-loaded*
    (gtk-ffi:load-gtk-libs)
    (setf *gtk-loaded* t))
  (gtk-reset))

(defun init-gtk (&key close-all-windows)
  "Replacement for cells-gtk-init. Threadsafe. Use to reset cells-gtk to a defined state."    
  (unless *gtk-loaded*                  ; make sure gtk is loaded
    (gtk-ffi:load-gtk-libs)
    (setf *gtk-loaded* t))
  (when close-all-windows
    (gtk-main-quit))
  (unless *gtk-initialized*    ; init only once
    (with-trcs
      #+cells-gtk-threads
      (progn
        (g-thread-init +c-null+)	; init threading
        (gdk-threads-init))
      (assert (gtk-init-check +c-null+ +c-null+))
      #+cells-gtk-opengl (gl-init)
      (gtk-reset)
      (setf *gtk-initialized* t))))

;;;  Instantiate and show app (show splash)

(defun show-win (app-name &rest initargs)
  "Show a window by instantiating app-name with initargs.  Load ressources and/or show
splash screen, if applicable.  A threadsafe replacement for \"make-be\".  To simulate a main-window, use
\":terminate-on-close t\".  Use start-win to make sure the background thread is running."
					; weird issue:  the window does not show up if we init it with :visible (c-in nil)
  					;               and there is no other window currently open
  					; --> we open another window (visible nil) to kick gtk
  (let ((app) (splash) (app-p (subtypep app-name 'gtk-app)))
    
    (with-trc (setf app (apply #'make-instance app-name :visible (c-in nil) initargs)))
    
    (when app-p
      (when (splash-screen-image app)
	(with-trcs
	  (setf splash (make-instance 'splash-screen :image-path (splash-screen-image app)
				      :visible (c-in nil)))
	  (gtk-window-set-auto-startup-notification nil)
	  (to-be splash)
	  (with-integrity (:change :make-splash-visible)
	   (setf (visible splash) t))
	  (not-to-be (make-instance 'window)) ; kick gtk ... ugly
	  (loop while (gtk-events-pending) do
	       (gtk-main-iteration)))))

    #+rc-functions-present?
    (when app-p
      (with-slots (rc-file) app
	(when (and rc-file (probe-file rc-file))
	  (with-trcs
	    (gtk-rc-reset-styles (gtk-settings-get-default))
	    (gtk-rc-parse rc-file)))))

    (with-trc (to-be app))
	
    (when splash
      (not-to-be splash)
      (gtk-window-set-auto-startup-notification t))

    (with-integrity (:change :make-app-visible)
     (setf (visible app) t))
    
    (not-to-be (make-instance 'window :visible nil))	; ph: kick gtk ... ugly
    app))


;;; Main loop
  
(defun main-loop ()
  "Run GTK Main until user signal quit.  Errors are caught and displayed in a dialog, providing the user with the option to \"recklessly continue\" -- not to be called directly"
  (unwind-protect
       (if (eql *using-thread* 'yes)
	   (loop until
		(restart-case
		    (handler-bind
			((gtk-continuable-error #'(lambda (condition) (invoke-restart  'continue-from-error condition )))
			 (error #'(lambda (con) (invoke-restart 'report-error con))))
		      #-lispworks
		      (gtk-main)
		      ;; Despite a workaround for Slime (see FAQ), the gtk-main-iteration-do loop, 
		      ;; still appears to be necessary for LW. Otherwise, LW consumes 99% of CPU.
		      #+lispworks	; give slime a chance.
		      (loop 
			 (loop while (gtk-events-pending) do 
			      (gtk-main-iteration-do nil))
			 (process-wait-with-timeout .01 "GTK event loop waiting"))
		      t)
		  ;; Restart cases
		  (continue-from-error (c1)
		    (trc "show message")
		    (show-message (format nil "Cells-GTK Error: ~a" c1)
				  :message-type :error :title "Cells-GTK Error")
		    (trc "showed the message"))
		  (report-error (c2)
		    (trc "show error message")
		    (when (eql (show-message (format nil "Lisp Error: ~a~%~%Recklessly continue?" c2)
					     :message-type :error
					     :title "Lisp Error"
					     :buttons-type :yes-no)
			       :no)
		      (trc ">>>> ERROR REPORTING -->" c2)
		      (error c2)))))
	   (gtk-main))

    ;; clean-up forms  -- application windows are taken down by gtk-quit-add callbacks
    (trc "cells-gtk clean-up code")
    (loop for i below (gtk-main-level)
       do (trc "  gtk-main-quit")
       do (gtk-main-quit))
    ;; Next is a work-around for a problem with gtk and lispwork-created .exe files
    #+(and Lispworks win32)(loop for i from 1 to 30 do (gtk-main-quit))
    (loop while (gtk-events-pending)
       do (trc "  gtk-main-iteration-do")
       do (gtk-main-iteration-do nil))))

;;;
;;; start-app
;;;

(defun start-app (app-name &key debug)
  "Start in application within the main thread (only return when application window is closed.
To run gtk in a background thread, use start-win instead."
  (let ((*gtk-debug* debug))
    (case *using-thread*
      ('yes (error "Cannot mix start-win and start-app in one lisp session.  Use start-win or restart lisp"))
      (t (setf *using-thread* 'no)))
    (with-trcs
      (init-gtk)
      (show-win app-name :terminate-on-close t)
      (when *gtk-debug* (trc nil "STARTING GTK-MAIN") (force-output))
      (main-loop)))
  0)


;;;
;;; Facilities to run gtk apps in an own thread
;;;

;;; Important:  GTK wants to communicate with a single thread per session
;;;             We create such a dispatcher thread and keep it running continously
;;;             in the background


;;; facilities to start/stop gtk-main thread

#+cells-gtk-threads
(progn
 (let ((gtk-main-thread nil))
   (flet ((gtk-thread ()
	    (gdk-threads-register-gtk-main (bt:current-thread))
	    (setf gtk-main-thread (bt:current-thread))
	    (loop (main-loop))))
     (defun gtk-main-thread ()
       "returns the gtk main thread -- not to be called directly"
       gtk-main-thread)
     (defun stop-gtk-main ()
       "Final clean up stuff -- need to RESTART lisp to access gtk again."
       ;        (terminate-gtk)
       (bt:destroy-thread gtk-main-thread))
     (defun start-gtk-main ()
       "Fire up gtk main thread in the background. -- not to be called directly."
       (if-bind (thread (find-if #'(lambda (thrd) (string= (bt:thread-name thrd) "gtk-main-thread")) (bt:all-threads)))
		(unless (eq thread gtk-main-thread)
		  #+msg (format t "~&INFO: there is another gtk-main thread.  Recover from recompiling cells-gtk in use.~%")
		  (gdk-threads-register-gtk-main thread)
		  (setf gtk-main-thread thread))
		(prog1
		    (let ((so *standard-output*))
		      (init-gtk)
		      #+msg (format t "~&starting gtk-main thread ...")
		      (bt:make-thread #'(lambda () (let ((*standard-output* so) (*print-readably* nil))
                                                     (gtk-thread))) :name "gtk-main-thread"))
		  (loop while (not gtk-main-thread)
		     do (bt:thread-yield))
		  #+msg (format t " done~%"))))
     (defun close-all-windows ()
       "close all open windows by issuing gtk-main-quit"
       (when gtk-main-thread
	 (gtk-main-quit)))))

 (defun start-win (app-class &rest initargs)
   "Starts app-class with initargs in its own thread.  Use :terminate-on-close t to close all other
 windows once this one is closed."
   (case *using-thread*
     ('no (error "Cannot mix start-win and start-app in one lisp session.  Use start-app or restart lisp"))
     (t (setf *using-thread* 'yes)))
   (start-gtk-main)
   (apply #'show-win app-class initargs)))

