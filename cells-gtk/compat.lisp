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

;;; From clocc port
;;; N.B.: There is no guarantee that the wait function
;;; will run in the stack group of the waiting process.  So
;;; you can't depend on dynamic bindings and catches being in effect.
(defun process-wait (whostate predicate &rest args)
  "Sleep until PREDICATE becomes true."
  #+Allegro (apply #'mp:process-wait whostate predicate args)
  #+CMU (mp:process-wait whostate (lambda () (apply predicate args)))
  #+LispWorks (apply #'mp:process-wait whostate predicate args))

;;; From clocc port, but with additions and deletions
(defun process-wait-with-timeout (timeout whostate 
                                  &optional (predicate #'(lambda () t) pred-supplied-p)
                                  &rest args)
  #+allegro (declare (ignore pred-supplied-p))
  "Sleep until PREDICATE becomes true, or for TIMEOUT seconds, whichever comes first."
  #+Allegro
  (apply #'mp:process-wait-with-timeout whostate timeout predicate args)
  #+CMU (mp:process-wait-with-timeout
         whostate timeout (lambda () (apply predicate args)))
  #+LispWorks
  (if pred-supplied-p
    (apply #'mp:process-wait-with-timeout whostate timeout predicate args)
    (mp:process-wait-with-timeout whostate timeout)))
