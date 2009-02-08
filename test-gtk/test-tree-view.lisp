(in-package :test-gtk)

(defmodel listbox-test-item ()
  ((string :accessor string$ :initarg :string :initform nil)
   (icon :accessor icon$ :initarg :icon :initform nil)
   (int :accessor int$ :initarg :int :initform nil)
   (float :accessor float$ :initarg :float :initform nil)
   (double :accessor double$ :initarg :double :initform nil)
   (boolean :accessor boolean$ :initarg :boolean :initform nil)
   (date :accessor date$ :initarg :date :initform nil)))

(defmethod print-object ((item listbox-test-item) stream)
  (with-slots (string icon int float double boolean date) item
    (format stream "~a| ~a| ~a| ~a| ~a| ~a| ~a" string icon int float double boolean date)))

;;;
;;; Some stuff for the cells-tree-view
;;; 

;;; a sample data structure

(defmodel node (family)
  ((val :accessor val :initarg :val :initform (c-in (random 100)))
   (writable :accessor writable :initarg :writable :initform (c-in (plusp (random 2))))))

(defun make-sample-tree (&optional (name "tree") (depth 3) parent)
  (make-instance 'node
		 :md-name name :fm-parent parent
		 :kids (c?n (when (plusp depth)
			      (loop for i from 0 below 3
				 collect (make-sample-tree (format nil "~a-~a" name i)
							   (1- depth)
							   self))))))


;;; a suitable cells-tree-view widget


(defmethod val-type ((self node))
  (type-of (val self)))

(def-cells-tree-view node-tree ()
  (val
   :title "Values"
   :type :string
   :writer-fn #'(lambda (val) (read-from-string val))
   :reader-fn #'(lambda (val) (prin1-to-string val))
   :render-fn #'(lambda (node) (if (writable node) '(:editable t :foreground "blue") '(:editable nil :foreground "black"))))
  (writable
   :title "Editable"
   :type :boolean
   :writer-fn #'identity
   :reader-fn #'identity
   :render-fn #'(lambda (node) (declare (ignore node)) '(:activatable t)))
  (val-type
   :title "Type"
   :reader-fn #'(lambda (val) (prin1-to-string (if (consp val) (car val) val)))))


(defmacro root ()
  '(data (upper self test-tree-view)))

(defmodel test-tree-view (notebook)
  ((data :accessor data :initform (c-in (make-sample-tree "tree" 3)))
   (items :accessor items :initarg :items 
	  :initform (c? (with-widget-value (hscale :hscale)
			  (loop for i from 1 to hscale collect
			       (make-be 'listbox-test-item
					:string (format nil "Item ~d" i)
					:icon (nth (random 5) (list "home" "open" "save" "ok" "cancel"))
					:int i 
					:float (coerce (* (+ i 1) (/ 1 (1+ (random 100)))) 'single-float) 
					:double (coerce (* (+ i 2) (/ 1 (1+ (random 1000)))) 'double-float) 
					:boolean (oddp i)
					:date (- (get-universal-time) (random 10000000))))))))
  (:default-initargs
      :tab-labels (list "Listbox" "Treebox" "Cells-Tree-View")
    :kids (kids-list?
           (mk-vbox 
            :homogeneous nil
            :kids (kids-list?
                   (mk-scrolled-window
                    :kids (kids-list?
                           (mk-listbox
                            :columns (def-columns
				       (:string (:title "Selection")))
                            :items (c? (with-widget-value (sel :listbox)
                                         (if (listp sel) sel (list sel))))
                            :print-fn (lambda (item)
					(list (format nil "~a" item))))))
                   (mk-frame 
                    :label "Selection mode"
                    :kids (kids-list?
                           (mk-hbox
                            :md-name :selection-mode
                            :kids (kids-list?
                                   (mk-radio-button :md-name :none :label "None"
						    :value (c-in t))
                                   (mk-radio-button :md-name :single :label "Single")
                                   (mk-radio-button :md-name :browse :label "Browse")
                                   (mk-radio-button :md-name :multiple :label "Multiple")))))
                   
                   (mk-hbox 
                    :kids (kids-list?
                           (mk-label :text "Select")
                           (mk-combo-box 
                            :md-name :selection-predicate
                            :init (c? (first (items self)))
                            :items (list
                                    #'null
                                    #'(lambda (itm)
                                        (declare (ignore itm))
                                        t)
                                    #'(lambda (itm) (not (null (boolean$ itm))))
                                    #'(lambda (itm) 
                                        (multiple-value-bind (sec min hour day month year) 
                                            (decode-universal-time (get-universal-time))
                                          (declare (ignore sec min hour day year))
                                          
                                          (multiple-value-bind (itm-sec itm-min itm-hour itm-day itm-month itm-year)
                                              (decode-universal-time (date$ itm))
                                            (declare (ignore itm-sec itm-min itm-hour itm-day itm-year))
                                            (= month itm-month))))
                                    #'(lambda (itm) (oddp (int$ itm)))
                                    #'(lambda (itm) (evenp (int$ itm))))
                            :print-fn (c?
					#'(lambda (item)
					    (case (position item (items self))
					      (0 "None")
					      (1 "All")
					      (2 "True")
					      (3 "This month")
					      (4 "Odd")
					      (5 "Even")))))
                           (mk-label :text "Items in Listbox")
                           (mk-hscale 
                            :md-name :hscale
                            :expand t :fill t
                            :min 0 :max 200
                            :init 5)))
                   (mk-scrolled-window
                    :kids (kids-list?
                           (mk-listbox
                            :md-name :listbox
                            :selection-mode (c? (widget-value :selection-mode))
                            :columns (def-columns
				       (:string (:title "String")
						#'(lambda (val)
						    (declare (ignore val))
						    '(:font "courier")))
				       (:icon (:title "Icon"))
                                       (:int (:title "Int") #'(lambda (val) 
                                                                (if (oddp val) 
                                                                    '(:foreground "red" :size 14)
								    '(:foreground "blue" :size 6))))
                                       (:float (:title "Float" :expand nil))
                                       (:double (:title "Double") #'(lambda (val)
                                                                      (if (> val 0.5)
                                                                          '(:foreground "cyan" :strikethrough nil)
									  '(:foreground "navy" :strikethrough t))))
                                       (:boolean (:title "Boolean"))
                                       (:date (:title "Date")))
                            :select-if (c? (widget-value :selection-predicate))
                            :items (c? (items (upper self test-tree-view)))
                            :print-fn (lambda (item)
					(list (string$ item) (icon$ item) (int$ item) (float$ item)
					      (double$ item) (boolean$ item) (date$ item))))))))
           (mk-vbox 
            :homogeneous nil
            :kids (kids-list?
                   (mk-scrolled-window
                    :kids (kids-list?
                           (mk-listbox
                            :columns (def-columns (:string (:title "Selection")))
                            :items (c? (with-widget-value (sel :treebox)
                                         (mapcar #'(lambda (item)
                                                     (list (format nil "~a" (class-name (class-of item)))))
						 (if (listp sel) sel (list sel))))))))
                   (mk-frame 
                    :label "Selection mode"
                    :kids (kids-list?
                           (mk-hbox
                            :md-name :tree-selection-mode
                            :kids (kids-list?
                                   (mk-radio-button :md-name :none :label "None"
						    :value (c-in t))
                                   (mk-radio-button :md-name :single :label "Single")
                                   (mk-radio-button :md-name :browse :label "Browse")
                                   (mk-radio-button :md-name :multiple :label "Multiple")))))
                   (mk-hbox 
                    :kids (kids-list?
                           (mk-label :text "Select")
                           (mk-combo-box 
                            :md-name :tree-selection-predicate
                            :init (c? (first (items self)))
                            :items (list
                                    #'null
                                    #'(lambda (itm) (subtypep (class-name (class-of itm)) 'vbox))
                                    #'(lambda (itm) (subtypep (class-name (class-of itm)) 'button))
                                    #'(lambda (itm) (subtypep (class-name (class-of itm)) 'notebook)))
                            :print-fn (c?
					#'(lambda (item)
					    (case (position item (items self))
					      (0 "None")
					      (1 "VBoxes")
					      (2 "Buttons")
					      (3 "Notebooks")))))))
                   (mk-scrolled-window
                    :kids (kids-list?
                           (mk-treebox
                            :md-name :treebox
                            :selection-mode (c? (widget-value :tree-selection-mode))
                            :select-if (c? (widget-value :tree-selection-predicate))
                            :columns (def-columns				    
				       (:string (:title "Widget class")
						#'(lambda (val)
						    (declare (ignore val))
						    '(:font "courier")))
				       (:icon (:title "Icon"))
                                       (:int (:title "Number of kids") 
					     #'(lambda (val)
						 (list :foreground (if (> val 5) "red" "blue"))))
                                       (:string (:title "Gtk address")))
                            :roots (c? (list (upper self gtk-app)))
                            :children-fn #'cells:kids
                            :print-fn #'(lambda (item) 
					  (list 
					   (format nil "~a" (class-name (class-of item)))
					   (case (class-name (class-of item))
					     (gtk-app "home")
					     (vbox "open")
					     (hbox "open")
					     (window "index")
					     (t "jump-to"))
					   (length (kids item))
					   (format nil "~a"
						   (when (subtypep (class-name (class-of item)) 'cells-gtk::gtk-object)
						     (cells-gtk::id item))))))))))
	   (mk-vbox
	    :expand t :fill t
	    :kids (kids-list?
		   (mk-label :text "Both tree views display the same data structure.  Changes to one will affect the other.")
		   (mk-hbox
		    :expand t :fill t
		    :kids (kids-list?
			   (mk-vbox
			    :expand t :fill t
			    :kids (kids-list?
				   (mk-label :text "Treeview 1")
				   (mk-hbox
				    :expand t :fill t
				    :kids (kids-list?
					   (mk-vbox
					    :kids (kids-list?
						   (mk-button
						    :label "New node"
						    :on-clicked (callback (w e d)
								  (with-widget (tree :tree-1)
								    (let ((node (or (value tree) (root))))
								      (with-integrity (:change 'tv-new-node)
									(push (make-instance 'node :fm-parent node) (kids node)))))))
						   (mk-button
						    :label "Delete"
						    :sensitive t
						    :on-clicked (callback (w e d)
								  (with-widget-value (node :tree-1)
								    (with-integrity (:change 'tv-del-node)
								      (setf (kids (upper node)) (remove node (kids (upper node))))))))))
					   (mk-scrolled-window
					    :expand t :fill t
					    :kids (kids-list? (mk-node-tree (root) :expand t :fill t :md-name :tree-1)))))))
			   (mk-vseparator)
			   (mk-vbox
			    :expand t :fill t
			    :kids (kids-list?
				   (mk-label :text "Treeview 2")
				   (mk-hbox
				    :expand t :fill t
				    :kids (kids-list?
					   (mk-scrolled-window
					    :expand t :fill t
					    :kids (kids-list? (mk-node-tree (root) :expand t :fill t :md-name :tree-2)))
					   (mk-vbox 
					    :kids (kids-list?
						   (mk-button
						    :label "New node"
						    :on-clicked (callback (w e d)
								  (with-widget (tree :tree-2)
								    (let ((node (or (value tree) (root))))
								      (with-integrity (:change 'tv-new-node)
									(push (make-instance 'node :fm-parent node) (kids node)))))))
						   (mk-button
						    :label "Delete"
						    :sensitive t
						    :on-clicked (callback (w e d)
								  (with-widget-value (node :tree-2)
								    (with-integrity (:change 'tv-del-node)
								      (setf (kids (upper node)) (remove node (kids (upper node))))))))
						   )))))))))))))

