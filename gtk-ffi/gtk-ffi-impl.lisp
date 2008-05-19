
#|

Implementation dependent stuff goes here

Currently supported

 -- sbcl: utf-8 string handling
 -- clisp: utf-8 string handling (thanks to Ingo Bormuth)

|#

(in-package :gtk-ffi)


;;;
;;; UTF-8 string handling
;;;

(defun lisp-to-utf-8 (str)
  #-(or clisp sbcl) (return-from lisp-to-utf-8 str)
  (when str
    #+clisp (ext:convert-string-to-bytes str charset:utf-8)
    #+sbcl (sb-ext:string-to-octets str :external-format :utf-8)))

(defun utf-8-to-lisp (str)
  #-(or clisp sbcl) (return-from utf-8-to-lisp str)
  (when str
    (let* ((nat (lisp-to-utf-8 str))
           (oct (coerce (loop for i from 0 below (length nat)
                           for b = (aref nat i)
                           collect b
                           ;; ph: gtk gives us 4 bytes per char ; why ?
                           if (= b 195) do (incf i 2))
                        '(vector (unsigned-byte 8)))))
      #+clisp (ext:convert-string-from-bytes oct charset:utf-8)
      #+sbcl  (sb-ext:octets-to-string oct :external-format :utf-8))))



