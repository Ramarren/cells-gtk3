
;;; Copyright (c) 2004 Kenny Tilton
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without restriction,
;;; including without limitation the rights to use, copy, modify,
;;; merge, publish, distribute, sublicense, and/or sell copies of the
;;; Software, and to permit persons to whom the Software is furnished
;;; to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR
;;; ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
;;; CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;-----------------------------------------------------------------------

;;;
;;; Kenny Tilton trace stuff.
;;;
(in-package :pod-utils)

(defparameter *trcdepth* 0)
(defvar *count* nil)
(defvar *counting* nil)
(defvar *dbg*)
(defvar *stop* nil)

(defun utils-kt-reset ()
  (setf *count* nil
    *stop* nil
    *dbg* nil
    *trcdepth* 0))

;----------- trc -------------------------------------------


(defmacro count-it (&rest keys)
  `(when *counting*
     (call-count-it ,@keys)))

(defmacro trc (tgt-form &rest os
                &aux (wrapper (if (macro-function 'without-c-dependency)
                                  'without-c-dependency 'progn)))
  (if (eql tgt-form 'nil)
      '(progn)
    (if (stringp tgt-form)
        `(,wrapper
          (call-trc t ,tgt-form ,@os))
      (let ((tgt (gensym)))
        `(,wrapper
          (bif (,tgt ,tgt-form)
            (if (trcp ,tgt)
                (progn
                  (assert (stringp ,(car os)))
                  (call-trc t ,@os)) ;;,(car os) ,tgt ,@(cdr os)))
              (progn
                ;;(break "trcfailed")
                (count-it :trcfailed)))
            (count-it :tgtnileval)))))))

(defun call-trc (stream s &rest os)
  (if #+cormanlisp nil #-cormanlisp (and (boundp '*trcdepth*)
          *trcdepth*)
        (format stream "~&~v,,,'.<~d~>> " (mod *trcdepth* 100) *trcdepth*)
      (format stream "~&"))
    (format stream "~a" s)
    (let (pkwp)
      (dolist (o os)
        (format stream (if pkwp " ~s" " | ~s") o)
        (setf pkwp (keywordp o))))
    (values))

(defun call-count-it (&rest keys)
    (declare (ignorable keys))
  ;;; (when (eql :TGTNILEVAL (car keys))(break))
  (let ((entry (assoc keys *count* :test #'equal)))
      (if entry
          (setf (cdr entry) (1+ (cdr entry)))
        (push (cons keys 1) *count*))))


;(export '(trc)) ; trc is now in cells