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

(defmacro gtk-report-error (type string &body args)
  `(error ',type :format-string ,string :format-arguments (list ,@args)))

(define-condition gtk-continuable-error (condition)
  ((text :initarg :text :reader text)))

(define-condition gtk-cells-error (gtk-continuable-error) 
  ((format-string :initarg :format-string :reader format-string)
   (format-arguments :initarg :format-arguments :reader format-arguments))
  (:report (lambda (c stream)
	     (apply #'format stream (format-string c) (format-arguments c)))))

(define-condition gtk-object-id-error (gtk-cells-error) ())

(define-condition gtk-user-signals-quit (condition) ())



