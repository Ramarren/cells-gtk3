(in-package :test-gtk)

(defmodel test-addon (notebook)
  ()
  (:default-initargs
      :tab-labels (list "Calendar" "Arrows")
    :kids (kids-list?
           (mk-vbox
            :kids (kids-list?
                   (mk-calendar :md-name :calendar
                     :init (encode-universal-time 0 0 0 6 3 1971))
                   (mk-label
                    :text (c? (when (value (fm^ :calendar))
                                (multiple-value-bind (sec min hour day month year) 
                                    (decode-universal-time (value (fm^ :calendar)))
                                    (declare (ignorable sec min hour))
                                  (format nil "Day selected ~a/~a/~a" day month year)))))))
           (mk-vbox
            :kids (kids-list?
                   (mk-arrow 
                    :type (c? (value (fm^ :type))))
                   (mk-frame
                    :label "Arrow type"
                    :kids (kids-list?
                           (mk-hbox
                            :md-name :type
                            :kids (kids-list?
                                   (mk-radio-button :md-name :up :label "Up")
                                   (mk-radio-button :md-name :down :label "Down")
                                   (mk-radio-button :md-name :left :label "Left")
                                   (mk-radio-button :md-name :right :label "Right" :init t))))))))))
