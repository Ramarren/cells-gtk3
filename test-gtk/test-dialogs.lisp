(in-package :test-gtk)

(defmodel test-message (button)
  ((message-type :accessor message-type :initarg :message-type :initform nil))
  (:default-initargs      
      :label (c? (string-downcase (symbol-name (message-type self))))
      :on-clicked (callback (widget signal data)
		     (setf (text (fm^ :message-response))
			   (format nil "Dialog response ~a"
				   (show-message (format nil "~a message" (label self)) :message-type (message-type self)))))))

(defmodel test-file-chooser-dialog (button)
  ((action :accessor action :initarg :action :initform nil))
  (:default-initargs
      :stock (c? (action self))
;      :label (c? (string-downcase (symbol-name (action self))))
      :on-clicked (callback (widget signal data)
                    (with-integrity (:change 'on-click-cb)
                      (setf (text (fm^ :file-chooser-response))
                        (format nil "File chooser response ~a"
                          (file-chooser :title (format nil "~a dialog" (action self))
                            :select-multiple (value (fm^ :select-multiple-files))
                            :action (action self))))))))
			    
(defmodel test-dialogs (vbox)
  ()
  (:default-initargs
      :kids (kids-list?
	     (mk-hbox
	      :kids (kids-list?
		     (append
		      (list 
		       (mk-button :label "Query for text"
				  :on-clicked 
				  (callback (w e d)
					    (with-integrity (:change 'q4text)
					      (let ((dialog
						     (show-message-dialog     
						      :md-name :rule-name-dialog
						      :message "Type something:"
						      :title "My Title"
						      :message-type :question
						      :buttons-type :ok-cancel
						      :content-area (mk-entry :auto-update t))))
						(setf (text (fm^ :message-response)) dialog))))))
		      (loop for message-type in '(:info :warning :question :error) collect
			   (make-kid 'test-message :message-type message-type)))))
             (mk-label :md-name :message-response)
             (mk-hbox
              :kids (kids-list?
                     (mk-check-button :md-name :select-multiple-files
				      :label "Select multiple")
                     (loop for action in '(:open :save :select-folder :create-folder) collect
			  (make-kid 'test-file-chooser-dialog :action action))))
             (mk-label :md-name :file-chooser-response)
             (mk-notebook
              :expand t :fill t
              :tab-labels (list "Open" "Save" "Select folder" "Create folder")
              :kids (kids-list?
                     (loop for action in '(:open :save :select-folder :create-folder) collect
			  (mk-vbox
			   :kids (kids-list?
				  (mk-file-chooser-widget :md-name action
							  :action action 
							  :expand t :fill t
							  :filters '(("All" "*") ("Text" "*.txt" "*.doc") ("Libraries" "*.so" "*.lib")) 
							  :select-multiple (c? (value (fm^ :multiple))))
				  (mk-check-button :label "Select multiple" :md-name :multiple)
				  (mk-label :text (c? (string (value (psib (psib))))))))))))))
