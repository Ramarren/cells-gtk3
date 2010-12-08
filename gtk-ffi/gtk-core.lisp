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

(load-gtk-libs)

(def-gtk-lib-functions :glib
    (g-free :void ((data gpointer)))
  (g-slist-free :void ((lst (:pointer gslist))))
  (g-timeout-add guint
		 ((milliseconds guint)
		  (func :pointer)
		  (data gpointer)))
  (g-locale-from-utf8 gtk-string
		      ((utf8-string gtk-string)
		       (len gssize)
		       (bytes-read (:pointer gsize))
		       (bytes-written (:pointer gsize))
		       (gerror :pointer)))
  (g-locale-to-utf8 :pointer
		    ((local-string gtk-string)
		     (len gssize)
		     (bytes-read (:pointer gsize))
		     (bytes-written (:pointer gsize))
		     (gerror :pointer))))

(def-gtk-lib-functions :gthread
    (g-thread-get-initialized gboolean nil)
    (g-thread-init :void ((vtable :pointer))))

(def-gtk-lib-functions :gdk
  (gdk-threads-init :void ())
  (gdk-threads-enter :void ())
  (gdk-threads-leave :void ())
  (gdk-flush :void ()))

(cffi:defcstruct g-value
  (g-type gtype)
  (g-data (:array g-value-data 2)))

(defmacro with-g-value ((var) &body body)
  `(call-with-g-value (lambda (,var) ,@body)))

(defun call-with-g-value (fn)
  (cffi:with-foreign-object (g-value 'g-value)
    (loop for i from 0 below (cffi:foreign-type-size 'g-value)
          do (setf (cffi:mem-aref g-value :uint8 i) 0))
    (funcall fn g-value)))

#+test
(def-gtk-lib-functions :gobject
    (g-value-set-string ((value c-pointer)
                         (str c-string))))

#+test
(def-gtk-function :gobject g-value-set-string
  :arguments ((value c-pointer) (str c-string))
  :return-type nil :call-direct t)

(def-gtk-lib-functions :gobject
  ;; memory managment
  (g-object-ref :pointer
                ((object :pointer)))
  (g-object-unref :void
                  ((object :pointer)))
  (g-object-ref-sink :pointer
                     ((object :pointer)))
  (g-object-is-floating gboolean
                        ((object :pointer)))
  ;; callbacks
  (g-cclosure-new :pointer
		  ((callback-f :pointer)
		   (user-data gpointer)
		   (destroy-data :pointer)))
  (g-cclosure-new-swap :pointer
		       ((callback-f :pointer)
			(user-data gpointer)
			(destroy-data :pointer)))
  (g-signal-connect-closure gulong
			    ((instance gpointer)
			     (detailed-signal gtk-string)
			     (closure :pointer)
			     (after gboolean)))
  (g-object-set-property :void
			 ((object :pointer)
			  (property-name gtk-string)
			  (value :pointer)))
  ;; gvalues
  (g-value-init :pointer ((value :pointer)
			  (type gtype)))
  (g-value-unset :void ((value :pointer)))
  (g-value-set-string :void
		      ((value :pointer)
                       (str :string)))
  (g-value-set-int :void ((value :pointer)
                          (int gint)))
  (g-value-set-long :void ((value :pointer) (long glong)))
  (g-value-set-boolean :void
		       ((value :pointer)
			(bool gboolean)))
  (g-value-set-float :void
		     ((value :pointer)
                      (float gfloat)))
  (g-value-set-double :void
		      ((value :pointer)
                       (double gdouble))))
