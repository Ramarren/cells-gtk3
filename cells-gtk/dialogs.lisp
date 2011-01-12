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





(def-widget message-dialog (window)
  ((message :accessor message :initarg :message :initform nil)
   (message-type :accessor message-type :initarg :message-type :initform :info)
   (fn-response :accessor fn-response :initarg :fn-response)
   (buttons :accessor buttons :initarg :buttons :initform nil)
   (buttons-id :accessor buttons-id :initarg :buttons-id :initform (c-in nil))
   (buttons-type :accessor buttons-type :initarg :buttons-type :initform (c? (if (eql (message-type self) :question) :yes-no :close)))
   (content-area :owning t :accessor content-area :initarg :content-area :initform nil))
  (markup)
  ()
  :position :mouse
  :fn-response (lambda (self response) (declare (ignore self)) (case response (-5 :ok) (-6 :cancel) (-7 :close) (-8 :yes) (-9 :no)))
  :new-args (c_1 (list +c-null+
		       2
		       (ecase (message-type self) (:info 0) (:warning 1) (:question 2) (:error 3))
		       (ecase (buttons-type self) (:none 0) (:ok 1) (:close 2) (:cancel 3) (:yes-no 4) (:ok-cancel 5))
		       (message self))))

(defmethod md-awaken :after ((self message-dialog))
  (trc "md-awaken :after"))

(export! show-dialog show-message show-message-dialog)

(defobserver content-area ((self message-dialog))
  (when new-value
    (trc "adding content area" self)
    (let ((vbox (gtk-dialog-get-content-area (id self))))
        (gtk-box-pack-start vbox (id new-value) nil nil 5))))

(defobserver buttons ((self message-dialog))
  (when new-value
    (with-integrity (:change 'msg-dlg-buttons)
     (setf (buttons-id self) (mapcar #'(lambda (b) (gtk-dialog-add-button (id self) (car b) (car (cdr b)))) new-value)))))


(defun show-dialog (dlg-class &rest inits)
  (let ((self (apply #'make-instance dlg-class :awaken-on-init-p t inits)))
    (wtrc (0 100 "processing dlg")
      (let* ((response (wtrc (0 100 "running dialog")
			 (gtk-dialog-run (id self))))
	     (result (funcall (fn-response self) self response)))
	(trc "showed dialog" response result)
	(with-slots (content-area) self
	  (when content-area
	    (trc "reading content area" (value content-area))
	    (setf result (value content-area))
	    (trc "forgetting content-area")
	    (not-to-be content-area)
	    #+not-necessary (gtk-object-forget (id content-area) content-area)))
	(trc "destroying self (not-to-be)")
	(not-to-be self)
	#+not-necessary (progn
			  (gtk-widget-destroy (id self))
			  (trc "forgetting self")
			  (gtk-object-forget (id self) self))
	result))))

(defun show-message (text &rest inits)
  (apply #'show-dialog 'message-dialog :message text inits))

(defun show-message-dialog (&rest inits)
  (apply #'show-dialog 'message-dialog inits))


(def-object file-filter ()
  ((mime-types :accessor mime-types :initarg :mime-types :initform nil)
   (patterns :accessor patterns :initarg :patterns :initform nil))
  (name)
  ())

(defobserver mime-types ((self file-filter))
  (dolist (mime-type new-value)
    (gtk-file-filter-add-mime-type (id self) mime-type)))

(defobserver patterns ((self file-filter))
  (dolist (pattern new-value)
    (gtk-file-filter-add-pattern (id self) pattern)))



(def-object file-chooser ()
  ((action :accessor action :initarg :action :initform nil)
   (action-id :accessor action-id
	      :initform (c? (ecase (action self)
			      (:open 0)
			      (:save 1)
			      (:select-folder 2)
			      (:create-folder 3))))
   (filters :accessor filters :initarg :filters :initform nil)
   (filters-ids :accessor filters-ids 
		:initform (c? (loop for filter in (filters self) collect
				   (id (make-instance 'file-filter :name (first filter) :patterns (rest filter)))))))
  (local-only select-multiple current-name filename
	      current-folder uri current-folder-uri use-preview-label filter)
  (selection-changed)
  :on-selection-changed (callback (widget signal data)
			  (with-integrity (:change 'selection-changed)
			    (trc "file-chooser setting value")
			    (if (select-multiple self)
				(setf (value self) (gtk-file-chooser-get-filenames-strs (id self)))
				(setf (value self) (gtk-file-chooser-get-filename (id self)))))))

(defobserver filters-ids ((self file-chooser))
  (dolist (filter-id new-value)
    (gtk-file-chooser-add-filter (id self) filter-id)))

(defobserver action ((self file-chooser))
  (when new-value
    (gtk-file-chooser-set-action (id self) (action-id self))))



(def-widget file-chooser-widget (file-chooser vbox)
  ()
  ()
  ()
  :new-args (c_1 (list (action-id self))))


(def-widget file-chooser-dialog (file-chooser message-dialog)
  ()
  ()
  ()
  :on-selection-changed nil
  :position :mouse
  :fn-response (lambda (self response)
		 (trc "fn response for file-chooser dialog")
		 (when (eql response -5)
		   (if (select-multiple self)
		       (gtk-file-chooser-get-filenames-strs (id self))
		       (gtk-file-chooser-get-filename (id self)))))
  :new-args (c_1 (list (title self)
		       +c-null+
		       (action-id self)
		       "gtk-cancel" -6 ;;response-cancel
		       (format nil "gtk-~a"
			       (string-downcase 
				(symbol-name
				 (if (eql (action self) :select-folder) 
				     :open
				     (if (eql (action self) :create-folder)
					 :apply
					 (action self))))))
		       -5 ;;response-ok
		       +c-null+)))

(defun file-chooser (&rest inits)
  (bwhen (fn-string (apply #'show-dialog 'file-chooser-dialog inits))
    (let ((fn (parse-namestring fn-string))
	  (action (getf inits :action)))
      (flet ((fail (format-string &rest format-args)
	       (show-message (apply #'format nil format-string format-args)
			     :title (format nil "File ~(~a~) error" action))
	       nil))
       (case action
	 (:open (or (and (file-namestring fn) (probe-file fn))
		    (fail "\"~a\" is not a valid filename." fn-string)))
	 (t fn-string))))))

