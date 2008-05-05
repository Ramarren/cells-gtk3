(in-package :test-gtk)


(defmodel test-display (vbox)
  ()                 
  (:default-initargs ;; g_timeout_add a function that will move the bar until the "Pulse" toggle is false.
      :value (c? (with-widget-value (val :pulse)
		   (with-widget-value (timeout :timeout)
		     (trc "ADDING TIMEOUT")
		     (timeout-add timeout 
				  (lambda ()
				    (with-widget (pbar :pbar2)
				      (pulse pbar))
				    (widget-value :pulse))))))
    :expand t :fill t
    :kids (kids-list?
           (mk-hbox
            :kids (loop for icon-size in '(:menu :small-toolbar :large-toolbar :button :dnd :dialog) 
		     collect (mk-image :stock :harddisk :icon-size icon-size)
		     collect (mk-image :stock :my-g :icon-size icon-size)))
           (mk-hseparator)
           (mk-aspect-frame 
            :ratio 1
            :kids (kids-list? 
                   (mk-image :width 200 :height 250
			     :filename (namestring *tst-image*))))
           (mk-hseparator)
           (mk-hbox 
            :kids (kids-list?							    
                   (mk-progress-bar :md-name :pbar
				    :fraction (c? (widget-value :fraction-value 1)))
                   (mk-hscale :md-name :fraction-value
			      :value-type 'single-float
			      :min 0 :max 1
			      :step 0.01
			      :init 0.5)
                   (mk-button :label "Show in status bar"
			      :on-clicked 
			      (callback (widget event data)
				(with-widget (w :statusbar)
				  (with-widget (pbar :pbar)
				   (push-message w (format nil "~a" (fraction pbar)))))))))
           (mk-hbox
            :kids (kids-list?
                   (mk-progress-bar :md-name :pbar2				      
				    :pulse-step (c? (widget-value :step .1))
				    :fraction (c-in .1))
                   (mk-toggle-button :md-name :pulse :label "Pulse")
                   (mk-label :text "Interval")
                   (mk-spin-button :md-name :timeout
				   :sensitive (c? (not (widget-value :pulse)))
				   :min 10 :max 1000
				   :init 100)
                   (mk-label :text "Pulse step")
                   (mk-spin-button :md-name :step
				   :value-type 'single-float
				   :min 0.01 :max 1 :step 0.01				     
				   :init 0.1)
                   (mk-image :md-name :pulse-image
			     :stock (c? (if (widget-value :pulse) :yes :no)))))
           (mk-alignment 
            :expand t :fill t
            :xalign 0 :yalign 1
            :xscale 1
            :kids (c? (the-kids
                       (mk-statusbar :md-name :statusbar)))))))
