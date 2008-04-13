#|

1. TRC is now back in the cells package. pod-utils no longer exports TRC. use pod::trc to get to it.
We could probably just drop TRC from pod-utils.

2. def-c-output is now defobserver. name change only.

3. md-value/.md-value is now value/.value

4. Use :owning option on cell slot to handle things like:

    popup
    tree-model

5. Tempted to not have id or new-args be cells. not sure why new-args even needs to be a slot.

6. In test-menus, opening menu2 (after making it visible and active) crashes the lisp.

|#

(in-package :cells-gtk)


(defun make-be (class &rest args)
  (let ((x (apply 'make-instance class args)))
   (md-awaken x)
   x))

(defun to-be (x) (md-awaken x) x)

(defmacro kids-list? (&rest body)
  `(c? (the-kids ,@body)))

(export '(make-be to-be kids-list?))