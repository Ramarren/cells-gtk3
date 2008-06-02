(in-package :cgtk)

(def-object action ()
  ((name :accessor name :initarg :name :initform nil)
   (accel :accessor accel :initarg :accel :initform nil)
   (visible :accessor visible :initarg :visible :initform (c-in t))
   (sensitive :accessor sensitive :initarg :sensitive :initform (c-in t))
   (label :accessor label :initarg :label :initform nil)
   (tooltip :accessor tooltip :initarg :tooltip :initform nil)
   (stock :accessor stock :initarg :stock :initform nil)
   (stock-id :accessor stock-id :initform (c? (when (stock self)
						(string-downcase (format nil "gtk-~a" (stock self)))))))
  ()
  ()
  :new-args (c_1 (list (name self) nil nil (stock-id self))))

(defobserver visible ((self action))
  (gtk-ffi::gtk-object-set-property (id self) "visible" 'boolean new-value))
(defobserver sensitive ((self action))
  (gtk-ffi::gtk-object-set-property (id self) "sensitive" 'boolean new-value))

(defobserver label ((self action))
  (when new-value
    (gtk-ffi::with-gtk-string (str new-value)
      (gtk-ffi::gtk-object-set-property (id self) "label" 'c-pointer str))))

(defobserver tooltip ((self action))
  (when new-value
    (gtk-ffi::with-gtk-string (str new-value)      
      (gtk-ffi::gtk-object-set-property (id self) "tooltip" 'c-pointer str))))

(def-object action-group ()
  ((name :accessor name :initarg :name :initform nil)
   (visible :accessor visible :initarg :visible :initform (c-in t))
   (sensitive :accessor sensitive :initarg :sensitive :initform (c-in t)))
  ()
  ()
  :new-args (c_1 (list (name self))))

(defobserver sensitive ((self action-group))
  (gtk-ffi::gtk-action-group-set-sensitive (id self) new-value))

(defobserver visible ((self action-group))
  (gtk-ffi::gtk-action-group-set-visible (id self) new-value))

(defobserver .kids ((self action-group))
  (dolist (kid old-value)
    (gtk-ffi::gtk-action-group-remove-action (id self) (id kid)))
  (dolist (kid new-value)
    (gtk-ffi::gtk-action-group-add-action-with-accel (id self) (id kid) (accel kid))))

(def-object ui-manager ()
  ((action-groups :accessor action-groups :initform (c-in nil))
   (add-tearoffs :accessor tearoffs :initarg :tearoffs :initform nil))
  ()
  ())

(defobserver tearoffs ((self ui-manager))
  (gtk-ffi::gtk-ui-manager-set-add-tearoffs (id self) new-value))

(defmethod add-action-group ((self ui-manager) (group action-group) &optional pos)
  (let ((grp (to-be group)))
    (trc nil "ADD-ACTION-GROUP" grp) (force-output)
    (gtk-ffi::gtk-ui-manager-insert-action-group (id self) (id group) (or pos (length (action-groups self))))
    (push grp (action-groups self))))


(defmodel test-actions (vbox)
  ()
  (:default-initargs
      :action-group (mk-action-group 
		     :name "Group 1"
		     :kids (kids-list?
			    (mk-action
			     :name "Action 1" :stock :cdrom :label "Action 1" :accel "<Control>a")
			    (mk-action
			     :name "Action 2" :stock :network :label "Action 2" :accel "<Control>b")))
			     
      :kids (kids-list?
	     (mk-label :text "Actions test"))))