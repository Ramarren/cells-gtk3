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
		  (data :pointer)))
  (g-locale-from-utf8 gtk-string
		      ((utf8-string gtk-string)
		       (len gssize)
		       (bytes-read (:pointer gsize))
		       (bytes-written (:pointer gsize))
		       (gerror :pointer)))
  (g-locale-to-utf8 gtk-string
		    ((local-string gtk-string)
		     (len gssize)
		     (bytes-read (:pointer gsize))
		     (bytes-written (:pointer gsize))
		     (gerror :pointer))))

(def-gtk-lib-functions :gthread (g-thread-init :void ((vtable :pointer))))

(def-gtk-lib-functions :gdk
  (gdk-threads-init :void ())
  (gdk-threads-enter :void ())
  (gdk-threads-leave :void ())
  (gdk-flush :void ()))

(cffi:defcunion g-value-data
  (v-int :int)
  (v-uint :unsigned-int)
  (v-long :long)
  (v-ulong :unsigned-long)
  (v-int64 :int64)
  (v-uint64 :uint64)
  (v-float :float)
  (v-double :double)
  (v-pointer :pointer))

(cffi:defcstruct g-value
  (g-type :uint)
  (g-data (:array g-value-data 2)))

(defmacro with-g-value ((var) &body body)
  `(call-with-g-value (lambda (,var) ,@body)))

(defun call-with-g-value (fn)
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (let ((gva (cffi:foreign-alloc 'g-value)))
    (locally #+sbcl(declare (sb-ext:muffle-conditions sb-ext:compiler-note))
             (unwind-protect
                  (progn
                    (setf (cffi:foreign-slot-value gva 'g-value 'g-type) 0)
                    (let ((data (cffi:foreign-slot-value gva 'g-value 'g-data)))
                      (dotimes (n 2)
                        (setf (cffi:foreign-slot-value
                               (aref data n)
                               'g-value-data 'v-uint64)
                              0)))
                    (funcall fn gva))
               (cffi:foreign-free gva)))))

#+test
(def-gtk-lib-functions :gobject
    (g-value-set-string ((value c-pointer)
                         (str c-string))))

#+test
(def-gtk-function :gobject g-value-set-string
  :arguments ((value c-pointer) (str c-string))
  :return-type nil :call-direct t)
(def-gtk-lib-functions :gobject
  ;; callbacks
  (g-cclosure-new :pointer
		  ((callback-f :pointer)
		   (user-data :pointer)
		   (destroy-data :pointer)))
  (g-cclosure-new-swap :pointer
		       ((callback-f :pointer)
			(user-data :pointer)
			(destroy-data :pointer)))
  (g-signal-connect-closure :unsigned-long
			    ((instance :pointer)
			     (detailed-signal gtk-string)
			     (closure :pointer)
			     (after gtk-boolean)))
  (g-object-set-valist :void
		       ((object :pointer)
			(first-prop gtk-string)
			(varargs :pointer)))
  (g-object-set-property :void
			 ((object :pointer)
			  (property-name gtk-string)
			  (value :pointer)))
  (g-value-init :pointer ((value :pointer)
			  (type :unsigned-long)))
  (g-value-unset :void ((value :pointer)))
  (g-value-set-string :void
		      ((value :pointer) (str gtk-string)))
  (g-value-set-int :void ((value :pointer) (int :int)))
  (g-value-set-long :void ((value :pointer) (long :long)))
  (g-value-set-boolean :void
		       ((value :pointer)
			(bool gtk-boolean)))
  (g-value-set-float :void
		     ((value :pointer) (float :float)))
  (g-value-set-double :void
		      ((value :pointer) (double :double))))
