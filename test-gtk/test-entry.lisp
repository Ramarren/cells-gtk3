(in-package :test-gtk)

(defmodel test-entry (vbox)
  ()
  (:default-initargs
      :kids (kids-list?	     
             (mk-vbox 
              :kids (test-entry-1))
             
             (mk-check-button :md-name :cool 
               :init t
               :label "Cool")
             (mk-frame
	      :markup (with-markup ()
			  (with-markup (:foreground :green)
			    "Colorful")
			"frame labels")
              :kids (test-entry-2))
             (mk-hbox
              :kids (kids-list?
                     (mk-spin-button :md-name :spin
                       :init 10)))
             (mk-hbox
              :kids (kids-list?
                     (mk-label :text "Entry completion test (press i)")
                     (mk-entry
                      :max-length 20
                      :completion (loop for i from 1 to 10 collect
                                        (format nil "Item ~d" i))))))))

(defun test-entry-1 ()
  (c? (the-kids
       (mk-label
        :expand t :fill t
        :markup (c? (with-markup (:font-desc "24") 
                      (with-markup (:foreground :blue 
						:font-family "Arial" 
						:font-desc (with-widget-value (s :spin :alternative 10)
							     (truncate s)))
                        (widget-value :entry))
                      (with-markup (:underline :double 
					       :weight :bold 
					       :foreground :red
					       :font-desc (with-widget-value (s :hscale :alternative 10)
							    (truncate s)))
                        "is")
                      (with-markup (:strikethrough (widget-value :cool))
                        "boring")
                      (with-markup (:strikethrough (not (widget-value :cool)))
                        "cool!")))
        :selectable t)
       (mk-entry :md-name :entry :auto-update t :init "Testing"))))

(defun test-entry-2 ()
  (c? (the-kids
       (mk-vbox
        :kids (c? (the-kids
                   (mk-hbox 
                    :kids (the-kids
                           (mk-check-button :md-name :sensitive 
                             :label "Sensitive")
                           (mk-check-button :md-name :visible
                             :init t
                             :label "Visible")))
                   (mk-hscale :md-name :hscale 
                     :visible (c? (widget-value :visible))
                     :sensitive (c? (widget-value :sensitive))
                     :expand t :fill t
                     :min 0 :max 100
                     :init 10)))))))

