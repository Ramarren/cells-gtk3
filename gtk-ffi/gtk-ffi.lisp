#|

 Gtk ffi

 Copyright (c) 2004 by Vasilis Margioulas <vasilism@sch.gr>

 You have the right to distribute and use this software as governed by
 the terms of the Lisp Lesser GNU Public License (LLGPL):

    (http://opensource.franz.com/preamble.html)

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 Lisp Lesser GNU Public License for more details.

|#

(in-package :gtk-ffi)

;;; POD throw-away utility to convert hello-c/uffi to cffi
#+nil
(defun gtk-lib2cffi (body)
  "Convert hello-c to uffi to cffi types. Swap order of arguments."
  (flet ((convert-type (type)
          (case type
            (c-string 'gtk-string)
            (boolean 'gtk-boolean)
            (t (cffi-uffi-compat::convert-uffi-type (ffi-to-uffi-type type))))))
  (dbind (ignore module &rest funcs) body
     (pprint `(,ignore
               ,module
               ,@(mapcar
                  #'(lambda (f)
                      (dbind (name args &optional return-type) f
                             ` (,name
                                    ,(if return-type
                                         (convert-type return-type)
                                       :void)
                                    ,(mapcar #'(lambda (a)
                                                 (list
                                                  (car a)
                                                  (convert-type (cadr a))))
                                             args))))
                      funcs))
             *standard-output*))))


(defvar *gtk-debug* nil)

(defun int-slot-indexed (obj obj-type slot index)
  (declare (ignorable obj-type))
  (cffi:mem-aref (cffi:foreign-slot-value obj obj-type slot) :int  index))

(defun (setf int-slot-indexed) (new-value obj obj-type slot index)
  (declare (ignorable obj-type))
  (setf (cffi:mem-aref (cffi:foreign-slot-value obj obj-type slot) :int index)
    new-value))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun gtk-function-name (lisp-name)
    (substitute #\_ #\- lisp-name))
  #+(or cmu clisp)(load-gtk-libs)
) ;eval

(eval-when (:compile-toplevel :load-toplevel)
  #+cells-gtk-threads
  (defparameter *without-gtk-lock* '("GTK-ADDS-G-THREAD-SUPPORTED"
                                     "G-THREAD-INIT"
                                     "GTK-MAIN-LEVEL"
                                     "GDK-THREADS-INIT"
                                     "GTK-INIT-CHECK"
                                     "GDK-THREADS-ENTER"
                                     "GDK-THREADS-LEAVE"
                                     "GDK-FLUSH"))
  (defparameter *without-debug-msg* '("GTK-EVENTS-PENDING"
                                      "GTK-MAIN-ITERATION-DO"
                                      "GDK-THREADS-ENTER"
                                      "GDK-THREADS-LEAVE"
                                      ))
  #+cells-gtk-threads
  (defun with-gdk-threads-p (sym)
    "returns t if sym needs to be wrapped in with-gdk-threads"
    (not (member (symbol-name sym) *without-gtk-lock* :test #'string=)))
  (defun with-debug-p (sym)
    "returns t if a debug message should be printed for sym in *gtk-debug* mode"
    (not (member (symbol-name sym) *without-debug-msg* :test #'string=))))

(defmacro def-gtk-function (library name return-type arguments)
  (declare (ignore library))
  (let* ((gtk-name$ (gtk-function-name (string-downcase (symbol-name name))))
         (gtk-name (intern (string-upcase gtk-name$))))
    `(progn
       (cffi:defcfun (,gtk-name$ ,gtk-name) ,return-type ,@arguments)
       (defun ,name ,(mapcar 'car arguments)
         (when *gtk-debug*
           ,(when (with-debug-p name)
              `(format *trace-output* "~%Calling (~A ~{~A~^ ~})"
                       ,(string-downcase (string name)) (list ,@(mapcar 'car arguments)))))
         (let ((result ,(let ((fn `(,gtk-name ,@(mapcar #'car arguments))))
                          #+cells-gtk-threads (if (with-gdk-threads-p name) `(with-gdk-threads ,fn) fn)
                          #-cells-gtk-threads fn)))
           (when *gtk-debug*
             ,(when (with-debug-p name)
                `(format *trace-output* "~%  (~A ~{~A~^ ~}) returns ~A"
                         ,(string-downcase (string name)) (list ,@(mapcar 'car arguments))
                         result)))
           result))
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (export ',name)))))

(defmacro def-gtk-lib-functions (library &rest functions)
  `(progn
     ,@(loop for function in functions collect
       (destructuring-bind (name return-type (&rest args)) function
         `(def-gtk-function ,library ,name ,return-type ,args)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro callback-function ((&rest arguments) &optional return-type)
    (declare (ignore arguments return-type))
    `'c-pointer))

(defmacro def-c-struct (struct-name &rest fields)
  (let ((slot-defs (loop for field in fields
                         collecting (destructuring-bind (name type) field
                                      (list name
                                            (intern (string-upcase
                                                     (format nil "~a-supplied-p" name)))
                                            (ffi-to-uffi-type type))))))
    `(progn
       (uffi:def-struct ,struct-name
         ,@(loop for (name nil type) in slot-defs
                 collecting (list name type)))
       ;; --- make-<struct-name> ---
       ,(let ((obj (gensym)))
          `(defun ,(intern (string-upcase (format nil "make-~a" struct-name)))
               (&key ,@(loop for (name supplied nil) in slot-defs
                             collecting (list name nil supplied)))
             (let ((,obj (uffi:allocate-foreign-object ',struct-name)))
               ,@(loop for (name supplied nil) in slot-defs
                       collecting `(when ,supplied
                                     (setf (cffi:foreign-slot-value ,obj ',struct-name ',name) ,name)))
               ,obj)))

       ;; --- accessors ---
       ,@(mapcar (lambda (slot-def &aux
                          (slot-name (car slot-def))
                          (accessor (intern (format nil "~a-~a" struct-name slot-name))))
                   `(progn
                      (defun ,accessor (self)
                        (cffi:foreign-slot-value self ',struct-name ',slot-name))
                      (defun (setf ,accessor) (new-value self)
                        (setf (cffi:foreign-slot-value self ',struct-name ',slot-name)
                              new-value))))
                 slot-defs))))

(def-c-struct gdk-event-button
  (type int)
  (window c-pointer)
  (send-event uint8)
  (time uint32)
  (x double-float)
  (y double-float)
  (axes (c-ptr double-float))
  (state uint)
  (button uint)
  (device c-pointer)
  (x_root double-float)
  (y_root double-float))

(def-c-struct gdk-event-key
  (type int)
  (window c-pointer)
  (send-event uint8)
  (time uint32)
  (state uint)
  (keyval uint)
  (length int)
  (string c-pointer)
  (hardware-keycode uint16)
  (group uint8))

(def-c-struct gdk-event-expose
  (type int)
  (window c-pointer)
  (send-event uint8)
  ;; This is probably wrong. alignment issues...
  (area-x int)
  (area-y int)
  (area-width int)
  (area-height int)
  (region c-pointer)
  (count int))

(def-c-struct gdk-event-motion
  (type int)
  (window c-pointer)
  (send-event uint8)
  (time int)
  (x double-float)
  (y double-float)
  (axes c-pointer)
  (state int)
  (is-hint uint16)
  (device c-pointer)
  (x-root double-float)
  (y-root double-float))

(defun event-type (event)
  (ecase event
    (-1 :nothing)
    (0 :delete)
    (1 :destroy)
    (2 :expose)
    (3 :notify)                         ; that is, pointer motion notify
    (4 :button_press)
    (5 :2button_press)
    (6 :3button_press)
    (7 :button_release)
    (8 :key_press)
    (9 :key_release)
    (10 :enter_notify)
    (11 :leave_notify)
    (12 :focus_change)
    (13 :configure)
    (14 :map)
    (15 :unmap)
    (16 :property_notify)
    (17 :selection_clear)
    (18 :selection_request)
    (19 :selection_notify)
    (20 :proximity_in)
    (21 :proximity_out)
    (22 :drag_enter)
    (23 :drag_leave)
    (24 :drag_motion)
    (25 :drag_status)
    (26 :drop_start)
    (27 :drop_finished)
    (28 :client_event)
    (29 :visibility_notify)
    (30 :no_expose)
    (31 :scroll)
    (32 :window_state)
    (33 :setting)))

(uffi:def-struct list-boolean
  (value :unsigned-int)
  (end :pointer-void))

(defmacro with-gtk-string ((var string) &rest body)
  `(let ((,var ,string))
     ,@body)
  #+not
  `(let ((,var (to-gtk-string ,string)))
     (unwind-protect
          (progn ,@body)
       (g-free ,var))))

(defun value-set-function (type)
  (ecase type
    (c-string #'g-value-set-string)
    (c-pointer #'g-value-set-string) ;; string-pointer
    (integer #'g-value-set-int)
    (single-float #'g-value-set-float)
    (double-float #'g-value-set-double)
    (boolean #'g-value-set-boolean)))

(defun value-type-as-int (type)
  (ecase type
    (c-string (* 16 4))
    (c-pointer (* 16 4)) ;; string-pointer
    (integer (* 6 4))
    (single-float (* 14 4))
    (double-float (* 15 4))
    (boolean (* 5 4))))

(def-c-struct type-val
  (type long)
  (val double-float)
  (val2 double-float))

(def-c-struct gslist
  (data c-pointer)
  (next c-pointer))

(def-c-struct gtk-tree-iter
  (stamp int)
  (user-data c-pointer)
  (user-data2 c-pointer)
  (user-data3 c-pointer))

(defmacro with-tree-iter ((iter-var) &body body)
  `(uffi:with-foreign-object (,iter-var 'gtk-tree-iter)
     (setf (cffi:foreign-slot-value ,iter-var 'gtk-tree-iter 'stamp) 0)
     (setf (cffi:foreign-slot-value ,iter-var 'gtk-tree-iter 'user-data) +c-null+)
     (setf (cffi:foreign-slot-value ,iter-var 'gtk-tree-iter 'user-data2) +c-null+)
     (setf (cffi:foreign-slot-value ,iter-var 'gtk-tree-iter 'user-data3) +c-null+)
     ,@body))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun as-gtk-type-name (type)
    (ecase type
      (:string 'c-string)
      (:icon 'c-string)
      (:int 'int)
      (:long 'long)
      (:date 'float)
      (:float 'single-float)
      (:double 'double-float)
      (:boolean 'boolean)))
  (defun as-gtk-type (type)
    (ecase type
      (:string (* 16 4))
      (:icon (* 16 4))
      (:int (* 6 4))
      (:long (* 8 4))
      (:date (* 14 4))
      (:float (* 14 4))
      (:double (* 15 4))
      (:boolean (* 5 4)))))

(defun col-type-to-ffi-type (col-type)
  (cdr (assoc col-type '((:string . c-string) ;;2004:12:15-00:17 was c-pointer
                         (:icon . c-pointer)
                         (:boolean . boolean)
                         (:int . int)
                         (:long . long)
                         (:date . single-float)
                         (:float . single-float)
                         (:double . double-float)))))

(defmacro deref-pointer-runtime-typed (ptr type)
  "Returns a object pointed"
  (declare (ignorable type))
  `(uffi:deref-pointer ,ptr ,type))

(defun cast (ptr type)
  (deref-pointer-runtime-typed ptr (ffi-to-uffi-type type)))
