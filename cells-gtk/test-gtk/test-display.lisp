(in-package :test-gtk)


(defmodel test-display (vbox)
  ()                 
  (:default-initargs ;; g_timeout_add a function that will move the bar until the "Pulse" toggle is false.
      :value (c? (when (value (fm-other :pulse))
                   (timeout-add (value (fm-other :timeout))
                     (lambda ()
                       (pulse (fm-other :pbar2))
                       (value (fm-other :pulse))))))
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
                     :fraction (c? (value (fm^ :fraction-value))))
                   (mk-hscale :md-name :fraction-value
                     :value-type 'single-float
                     :min 0 :max 1
                     :step 0.01
                     :init 0.5)
                   (mk-button :label "Show in status bar"
                     :on-clicked 
                     (callback (widget event data)
                       (push-message (fm-other :statusbar)
                         (format nil "~a" (fraction (fm-other :pbar))))))))
           (mk-hbox
            :kids (kids-list?
                   (mk-progress-bar :md-name :pbar2				      
                     :pulse-step (c? (value (fm^ :step)))
                     :fraction (c-in .1))
                   (mk-toggle-button :md-name :pulse :label "Pulse")
                   (mk-label :text "Interval")
                   (mk-spin-button :md-name :timeout
                     :sensitive (c? (not (value (fm^ :pulse))))
                     :min 10 :max 1000
                     :init 100)
                   (mk-label :text "Pulse step")
                   (mk-spin-button :md-name :step
                     :value-type 'single-float
                     :min 0.01 :max 1 :step 0.01				     
                     :init 0.1)
                   (mk-image :md-name :pulse-image
                     :stock (c? (if (value (fm^ :pulse)) :yes :no)))))
           (mk-alignment 
            :expand t :fill t
            :xalign 0 :yalign 1
            :xscale 1
            :kids (c? (the-kids
                       (mk-statusbar :md-name :statusbar)))))))
