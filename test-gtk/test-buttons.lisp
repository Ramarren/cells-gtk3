(in-package :test-gtk)

(defmodel test-buttons (vbox)
  ((nclics :accessor nclics :initform (c-in 0)))
  (:default-initargs
      :kids (c? (the-kids
                 (mk-label :text (c? (trc "### executing toggled button rule")
				     (format nil "Toggled button active = ~a"
					     (with-widget (w :toggled-button)
					       (trc "   FOUND WIDGET" w (value w))
					       (value w)))))
                 (mk-hseparator)
                 (mk-label :text (c? (format nil "Check button checked = ~a" 
					     (widget-value :check-button))))
                 (mk-hseparator)
                 (mk-label :text (c? (trc "### executing radio button rule")
				     (format nil "Radio button selected = ~a" 
					     (with-widget (w :radio-group)
					       (trc "   FOUND WIDGET")
					       (value w)))))
                 (mk-hseparator)
                 (mk-label :text (c? (format nil "Button clicked ~a times" 
					     (nclics (upper self test-buttons))))
			   :selectable t)
                 (mk-hseparator)
                 
                 (mk-hbox
                  :kids (c? (the-kids
                             (mk-button :stock :apply
					:tooltip "Click ....."
					:on-clicked (callback (widget event data)
						      (incf (nclics (upper self test-buttons)))))
                             (mk-button :label "Continuable error"
					:on-clicked (callback (widget event data)
						      (trc "issuing continuable error" widget event)
						      (error 'gtk-continuable-error :text "Oops!")))
			     (mk-button :label "Lisp error (Div 0)"
					:on-clicked (callback (widget event data)
						      (print (let ((a 3)
                                                                   (b 0))
                                                               (/ a b)))))
                             (mk-toggle-button :md-name :toggled-button
					       :markup (c? (with-markup (:foreground (if (value self) :red :blue))
							     "_Toggled Button")))
                             (mk-check-button :md-name :check-button				      
					      :markup (with-markup (:foreground :green)
							"_Check Button")))))
                 (mk-hbox
                  :md-name :radio-group
                  :kids (kids-list?
			 (mk-radio-button :md-name :radio-1
					  :label "Radio 1")
			 (mk-radio-button :md-name :radio-2
					  :label "Radio 2" :init t)
			 (mk-radio-button :md-name :radio-3
					  :label "Radio 3")))
		 (mk-hbox
		  :kids (kids-list?
			 (mk-label :text (c? (trc "### executing toggled button rule 2")
					     (format nil "Toggled button active = ~a"
						     (with-widget (w :toggled-button)
						       (trc "   FOUND WIDGET 2" w (value w))
						       (value w)))))))))))
