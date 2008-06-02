(in-package :cgtk)

(defun register-callback (self callback-id fun)
  (let ((id (intern (string-upcase
                     (format nil "~a.~a" (id self) callback-id)))))
    (trc nil "registering callback" self :id id)
    (setf (gethash id (callbacks (nearest self gtk-app))) (cons fun self))
    id))

(defun dispatch-callback (gtk-app callback)
  (destructuring-bind (callback-id &rest callback-args) callback
    (let ((func-self (gethash callback-id (callbacks gtk-app))))
      ;(format t "sym:~S fun:~A~%" sym func-self)
      ;(force-output)
      (when (not func-self)
        (when *gtk-debug*
          (format t "~&callback ~a, type ~a, pkg ~a, not found. known callbacks:"
            callback-id (type-of callback-id) (when (typep callback-id 'symbol)
                                                (symbol-package callback-id)))
          (maphash (lambda (key func-self)
                     (declare (ignore func-self))
                     (format t "~&known callback key ~a, type ~a, pkg ~a"
                       key (type-of key)(when (typep key 'symbol) (symbol-package key))))
            (callbacks gtk-app))))
      (when (car func-self)
        (apply (car func-self) (cdr func-self) callback callback-args)))))

(defun gtk-callback (self event fn)
  (format nil "gtk_server_connect(~A, ~A, :callback ~A)"
	  (id self) event (register-callback self event fn)))

(defobserver bindings () ;;; (w widget) event fun)
  (loop for binding in new-value
        do (destructuring-bind (event . fn) binding
             (declare (ignorable event))
	     (gtk-send self  (gtk-callback self event fn)))))



