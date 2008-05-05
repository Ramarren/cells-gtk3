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


(def-gtk-lib-functions :glib
  (g-free :void ((data :pointer)))
  (g-slist-free :void ((lst :pointer)))
  (g-timeout-add :unsigned-int
		 ((milliseconds :unsigned-int)
		  (func :pointer)
		  (data :pointer)))
  (g-locale-from-utf8 gtk-string
		      ((utf8-string :pointer)
		       (len :int)
		       (bytes-read :pointer)
		       (bytes-written :pointer)
		       (gerror :pointer)))
  (g-locale-to-utf8 :pointer
		    ((local-string gtk-string)
		     (len :int)
		     (bytes-read :pointer)
		     (bytes-written :pointer)
		     (gerror :pointer))))

(def-gtk-lib-functions :gthread (g-thread-init :void ((vtable :pointer))))

(def-gtk-lib-functions :gdk
  (gdk-threads-init :void ())
  (gdk-threads-enter :void ())
  (gdk-threads-leave :void ())
  (gdk-flush :void ()))

(uffi:def-union g-value-data
  (v-int :int)
  (v-uint :unsigned-int)
  (v-long :long)
  (v-ulong :unsigned-long)
  (v-int64-lo :int)
  (v-int64-hi :int)
  (v-uint64-lo :unsigned-int)
  (v-uint64-hi :unsigned-int)
  (v-float :float)
  (v-double :double)
  (v-pointer :pointer-void))

(uffi:def-struct g-value
    (g-type (:array :int 16)))

(defmacro with-g-value ((var) &body body)
  `(call-with-g-value (lambda (,var) ,@body)))

(defun call-with-g-value (fn)
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (let ((gva (cffi:foreign-alloc 'g-value)))
    (unwind-protect
	 (dotimes (n 16)
	   (let* ((gv (cffi:mem-aref gva 'g-value 0))
		  (ns (cffi:foreign-slot-value gv 'g-value 'g-type)))
	     (setf (cffi:mem-aref ns ':int n) 0))))
    (locally
	#+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
      (funcall fn gva))
    (cffi:foreign-free gva)))

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
