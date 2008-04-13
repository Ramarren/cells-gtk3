
#|

Implementation dependent stuff goes here

Currently supported

 -- sbcl: utf-8 string handling

|#

(in-package :gtk-ffi)


;;;
;;; UTF-8 string handling
;;;

(defun utf-8-to-lisp (str)
  (when str
   #+sbcl (let ((s (sb-ext:string-to-octets str :external-format :utf-8)))
	    (sb-ext:octets-to-string 
	     (coerce (loop for i from 0 below (length s)
			for b = (aref s i)
			collect b
			if (= b 195) do (incf i 2)) ; ph: gtk gives us 4 bytes per char -- no idea why.  
		     '(vector (unsigned-byte 8)))
	     :external-format :utf-8))
   #-(or sbcl) str))

(defun lisp-to-utf-8 (str)
  (when str
    #+sbcl (sb-ext:string-to-octets str :external-format :utf-8)
    #-(or sbcl) str))


