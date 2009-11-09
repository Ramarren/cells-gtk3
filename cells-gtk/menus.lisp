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

(defmacro with-tree-iters (vars &body body)
  `(let (,@(loop for var in vars collect `(,var (gtk-adds-tree-iter-new))))
     (unwind-protect 
       (progn ,@body)
       ,@(loop for var in vars collect `(gtk-tree-iter-free ,var)))))

;;; ============= Combo-box ============================
;;; User should specify exactly one of :items or :roots 
;;; If specify :roots, specify :children-fn too.
(def-widget combo-box (container)
  ((items :accessor items :initarg :items :initform nil)
   (print-fn :accessor print-fn :initarg :print-fn
	     :initform #'(lambda (item) (format nil "~a" item))) ; see below if :roots
   (init :accessor init :initarg :init :initform nil)
   (roots :accessor roots :initarg :roots :initform nil)
   (children-fn :accessor children-fn :initarg :children-fn :initform #'(lambda (x) (declare (ignore x)) nil))
   (tree-model :cell nil :accessor tree-model :initform nil)
   (current-pos :accessor current-pos :initarg :current-pos :initform (c-in nil)))
  (active)
  (changed)
  :new-tail '-text
  :on-changed 
  (callback (widget event data)
					;(trc nil "combo-box onchanged cb" widget event data (id self))
	    (if (items self) 
		;; flat model (:items specified)
		(let ((pos (gtk-combo-box-get-active (id self))))
		  ;;(trc nil "combo-box pos" pos)
		  (setf (value self) (and (not (= pos -1))
					  (nth pos (items self))))
		  (setf (current-pos self) pos))
		;; non-flat tree-model (:roots specified)
		(with-tree-iters (iter)
		  (when (gtk-combo-box-get-active-iter (id self) iter)
		    (setf (value self)
			  (item-from-path
			   (children-fn self)
			   (roots self)
			   (read-from-string
			    (gtk-tree-model-get-cell (id (tree-model self)) iter 1 :string)))))))))


(defobserver current-pos ((self combo-box))
  (when new-value (gtk-combo-box-set-active (id self) new-value)))


(defmethod cgtk-set-active-item-by-path ((self combo-box) path)
  "Sets the value of the tree-model type combo-box to the item at the path. Path is a list of integers."
  (when-bind (tree (tree-model self))
    (with-tree-iters (it)
       (when (gtk-tree-model-get-iter-from-string (id tree) it (format nil "~{~A~^:~}" path))
	 (gtk-combo-box-set-active-iter (id self) it)
	 ;(break "in cgtk setting path = ~A" path)
	 (setf (value self) (item-from-path (children-fn self) (roots self) path))))))

;;; When user specifies :roots, he is using a tree-model.
;;; POD There is probably no reason he has to use :strings for the "columns"
(defobserver roots ((self combo-box))
  (when old-value
    (gtk-tree-store-clear (id (tree-model self))))
  (when new-value
    (with-integrity (:change 'combo-box-tree-model)
      (unless (tree-model self)
        (let ((model (mk-tree-store :item-types '(:string :string))))
          (setf (tree-model self) model)
          (setf (of-tree model) self)
          (gtk-combo-box-set-model (id self) (id model))))
      (let* ((user-print-fn (print-fn self)) ; because he shouldn't need to know this detail.
             (pfunc #'(lambda (x) (list (funcall user-print-fn x)))))
        (loop for root in new-value
            for index from 0 do
              (gtk-tree-store-set-kids (id (tree-model self)) root +c-null+ index
                '(:string :string) pfunc (children-fn self)))
        ;; Spec says iter must correspond to a path of depth one. But then there would be no point 
        ;; in set-active-iter.  Well, the spec seems to be wrong (or poorly worded).
        (when-bind (path (init self))
          (cgtk-set-active-item-by-path self path))))))

(defobserver items ((self combo-box))
  (when old-value
    (dotimes (i (length old-value))
      (gtk-combo-box-remove-text (id self) 0)))
  (when new-value
    (dolist (item (items self))
      (gtk-combo-box-append-text (id self) (funcall (print-fn self) item)))
    (with-integrity (:change 'combobox-items-set-value)
     (if-bind (index (position (init self) (items self)))
	      (progn (gtk-combo-box-set-active (id self) index)
		     (setf (value self) (init self)))
	      (progn (gtk-combo-box-set-active (id self) 0)
		     (setf (value self) (car (items self))))))))

;;; ============= Combo-box-entry ============================
(def-widget combo-box-entry (combo-box)
  ()
  (text-column)
  ())

;;; ============= Toolbar/Toolbutton ============================	
(def-object tooltips ()
  () () ())

(def-widget toolbar (container)
  ((orientation :accessor orientation :initarg :orientation :initform (c-in nil))
   (style :accessor style :initarg :style :initform (c-in nil)))
  (show-arrow tooltips)
  ()
  :padding 0)

(defobserver .kids ((self toolbar))  
  (when new-value
    (loop for item in new-value
	  for pos from 0 do
	  (gtk-toolbar-insert (id self) (id item) pos))))

(defobserver orientation ((self toolbar))
  (when new-value
    (gtk-toolbar-set-orientation (id self)
	  (case new-value
	    (:horizontal 0)
	    (:vertical 1)
	    (t 0)))))

(defobserver style ((self toolbar))
  (when new-value
    (gtk-toolbar-set-style (id self)
	  (case new-value
	    (:icons 0)
	    (:text 1)
	    (:both 2)
	    (:both-horiz 3)
	    (t 0)))))

(def-widget tool-item (container)
  ()
  (homogeneous expand is-important)
  ())

(defobserver .kids ((self tool-item))
  (assert-bin self)
  (when new-value
    (dolist (kid new-value)
      (gtk-container-add (id self) (id kid)))))

(def-widget separator-tool-item (tool-item)
  ()
  (draw)
  ())

(def-widget tool-button (tool-item)
  ((stock :accessor stock :initarg :stock :initform (c-in nil))
   (label :accessor label :initarg :label :initform (c-in nil))
   (icon-widget :owning t :accessor icon-widget :initarg :icon-widget :initform (c-in nil))
   (label-widget :owning t :accessor label-widget :initarg :label-widget :initform (c-in nil)))
  (use-underline stock-id)
  (clicked)
  :new-args (list +c-null+ +c-null+))


(defobserver icon-widget ((self tool-button))
  (when new-value
    (gtk-tool-button-set-icon-widget (id self) (id new-value))))

(defobserver label-widget ((self tool-button))
  (when new-value
    (gtk-tool-button-set-label-widget (id self) (id new-value))))

(defobserver label ((self tool-button))
  (when new-value
    (gtk-tool-button-set-label (id self) new-value)))

(defobserver stock ((self tool-button))
  (when new-value
    (setf (stock-id self) (string-downcase (format nil "gtk-~a" new-value)))))

;;; ============= Menu ============================	
(def-widget menu-shell (container)
  () () ()
  :padding 0)

(defobserver .kids ((self menu-shell))  
  (when new-value
    (dolist (kid new-value)
      (gtk-menu-shell-append (id self) (id kid)))))

(def-widget menu-bar (menu-shell)
  () () ())

(def-widget menu (menu-shell)
  ((owner :initarg :owner :accessor owner :initform (c-in nil)))
  (title)
  ())

(def-widget menu-item (item)
  ((label :accessor label :initarg :label :initform (c-in nil))
   (label-widget :owning t :accessor label-widget :initarg :label-widget :initform nil)
   (accel-label-widget :owning t :accessor accel-label-widget
		       :initform (c? (and (label self)
					  (let ((*parent* self))
					    (mk-accel-label :text (label self))))))
   (accel :accessor accel :initarg :accel :initform (c-in nil))
   (owner :initarg :owner :accessor owner :initform (c-in nil))
   (submenu-id :initarg :submenu-id :cell nil :accessor submenu-id :initform nil)) ; gtk-menu-item-get-submenu not doing it. POD
  (right-justified)
  (activate)
  :right-justified nil)


(defobserver .kids ((self menu-item))
  (when old-value			; pod never occurs ?
    (gtk-menu-item-remove-submenu (id self))) ;; almost certainly wrong -- better to Just Break here?
  (when new-value
    (gtk-menu-item-set-submenu (id self) (setf (submenu-id self) (gtk-menu-new)))
    (dolist (kid new-value)
      (gtk-menu-shell-append (submenu-id self) (id kid)))))


(defun accel-key-mods (accel)
  (destructuring-bind (key &rest mods-lst) accel
    (let ((mods 0))
      (when mods-lst
	(dolist (mod mods-lst)
	  (setf mods (logior mods
			     (ash 1 (ecase mod
				      (:shift 0)
				      (:control 2)
				      (:alt 3)))))))
      (values (char-int key) mods))))

(defobserver accel ((self menu-item))
  (when new-value
    (bwhen (win (upper self window))
     (multiple-value-bind (key mods) (accel-key-mods new-value)
       (gtk-widget-add-accelerator (id self) "activate" (accel-group win) key mods 1)))))

(defobserver label-widget ((self menu-item))
  (when new-value
    (gtk-container-add (id self) (id new-value))))
    
(defobserver accel-label-widget ((self menu-item))
  (let ((id (id self)))
    (let ((child (gtk-bin-get-child id)))
      (unless (cffi:null-pointer-p child)
        (gtk-container-remove id child)))
    (when new-value
      (gtk-accel-label-set-accel-widget (id new-value) id)
      (gtk-container-add id (id new-value)))))

(def-widget check-menu-item (menu-item)
  ((init :accessor init :initarg :init :initform nil))
  (active)
  (toggled)
  :active (c-in nil)
  :on-toggled (callback (widget event data)
                (trc nil "on-toggled" self widget event data)
                (with-integrity (:change 'check-menu-item-on-tog)
                  (let ((state (gtk-check-menu-item-get-active widget)))
                    (setf (value self) state)))))

(defobserver init ((self check-menu-item))
  (setf (active self) new-value)
  (setf (value self) new-value))

(def-widget radio-menu-item (check-menu-item)
  () () ()
  :new-tail (c? (let ((in-group-p (upper self menu-item))
		      (not-first-p (not (eql (first (kids (fm-parent self))) self))))
		      (when (and in-group-p  not-first-p)
			'-from-widget)))
			 
  :new-args (c_1 (let ((in-group-p (upper self menu-item))
		      (not-first-p (not (eql (first (kids (fm-parent self))) self))))		  
		      (if (and in-group-p not-first-p)
			  (list (id (first (kids (fm-parent self)))))			  
			  (list +c-null+)))))
  
(defobserver .value ((self radio-menu-item))
  (with-integrity (:change 'radio-menu-item-value)
   (when (and new-value (upper self menu-item))
     (setf (value (upper self menu-item)) (md-name self)))))

(def-widget image-menu-item (menu-item)
  ((stock :accessor stock :initarg :stock :initform nil)
   (image :owning t :accessor image :initarg :image :initform nil))
  ()
  ()
  :new-tail (c? (when (stock self)
		  '-from-stock))
  :new-args (c_1 (when (stock self)
		  (list (string-downcase (format nil "gtk-~a" (stock self))) +c-null+))))


(defobserver image ((self image-menu-item))
  (when new-value
    (gtk-image-menu-item-set-image (id self) (id new-value))))

(def-widget separator-menu-item (menu-item)
  () () ())
(def-widget tearoff-menu-item (menu-item)
  () () ())

