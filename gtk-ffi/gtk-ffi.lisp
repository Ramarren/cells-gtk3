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

;;; def-c-struct

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun compute-slot-def (field)
    (destructuring-bind (name type) field
      (list name (intern (string-upcase (format nil "~a-supplied-p" name))) type))))

(defmacro def-c-struct (struct-name &rest fields)
  (let ((slot-defs (mapcar #'compute-slot-def fields)))
    `(progn
       (cffi:defcstruct ,struct-name
         ,@(loop for (name nil type) in slot-defs
                 collecting (list name type)))
       ;; --- make-<struct-name> ---
       ,(let ((obj (gensym)))
          `(defun ,(intern (string-upcase (format nil "make-~a" struct-name)))
               (&key ,@(loop for (name supplied nil) in slot-defs
                             collecting (list name nil supplied)))
             (let ((,obj (cffi:foreign-alloc ',struct-name)))
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
  (type gdk-event-type)
  (window :pointer)
  (send-event gint8)
  (time guint32)
  (x gdouble)
  (y gdouble)
  (axes (:pointer gdouble))
  (state guint)
  (button guint)
  (device :pointer)
  (x_root gdouble)
  (y_root gdouble))

(def-c-struct gdk-event-key
  (type gdk-event-type)
  (window :pointer)
  (send-event gint8)
  (time guint32)
  (state guint)
  (keyval guint)
  (length gint)
  (string :pointer)
  (hardware_keycode guint16)
  (group guint8)
  (is_modifier guint8) ; actually a bitfield?
  )

(def-c-struct gdk-event-expose
  (type gdk-event-type)
  (window :pointer)
  (send-event gint8)
  ;; This is probably wrong. alignment issues...
  (area-x gint)
  (area-y gint)
  (area-width gint)
  (area-height gint)
  (region :pointer)
  (count gint))

(def-c-struct gdk-event-motion
  (type gdk-event-type)
  (window :pointer)
  (send-event gint8)
  (time guint32)
  (x gdouble)
  (y gdouble)
  (axes (:pointer gdouble))
  (state guint)
  (is_hint gint16)
  (device :pointer)
  (x_root gdouble)
  (y_root gdouble))

(defun value-set-function (type)
  (ecase type
    (gtk-string #'g-value-set-string)
    (gpointer #'g-value-set-string) ;; string-pointer
    (gint #'g-value-set-int)
    (gfloat #'g-value-set-float)
    (gdouble #'g-value-set-double)
    (gboolean #'g-value-set-boolean)))

(defun value-type-as-int (type)
  (ecase type
    (gtk-string (* 16 4))
    (gpointer (* 16 4)) ;; string-pointer
    (gint (* 6 4))
    (gfloat (* 14 4))
    (gdouble (* 15 4))
    (gboolean (* 5 4))))

(def-c-struct gslist
  (data gpointer)
  (next gpointer))

(defmacro with-tree-iter ((iter-var) &body body)
  `(let ((,iter-var (gtk-adds-tree-iter-new)))
     ,@body))

(defun as-gtk-type-name (type)
  (ecase type
    (:string 'gtk-string)
    (:icon 'gtk-string)
    (:int 'gint)
    (:long 'glong)
    (:date 'gfloat)
    (:float 'gfloat)
    (:double 'gdouble)
    (:boolean 'gboolean)))

(defun as-gtk-type (type)
  (ecase type
    (:string (* 16 4))
    (:icon (* 16 4))
    (:int (* 6 4))
    (:long (* 8 4))
    (:date (* 14 4))
    (:float (* 14 4))
    (:double (* 15 4))
    (:boolean (* 5 4))))
