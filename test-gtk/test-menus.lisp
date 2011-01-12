(in-package :test-gtk)

(defmodel test-menus (vbox)
  ()
  (:default-initargs
      :kids (kids-list?
             (mk-menu-bar 
              :kids (kids-list?
                     (mk-menu-item 
                      :md-name 'menu-1
                      :label "Menu 1"
                      :kids (kids-list?
                             (mk-image-menu-item 
                              :label "Save" 
                              :accel '(#\s :control :shift :alt)
                              :image (mk-image :stock :save :icon-size :menu)
                              :on-activate (callback (widget event data)
                                             (trc "TST SAVE") (force-output)))
                             (mk-menu-item
                              :md-name 'SUBMENU-MENUITEM
                              :label "Submenu"
                              :kids (c? (the-kids
                                            (mk-menu-item
                                             :md-name (gensym "SUBITEM-1")
                                             :label "subitem1"
                                             :on-activate (callback (widget event data)
                                                            (trc "dribble SAVE") #-iamnotkenny (dribble)))
                                            (mk-menu-item
                                             :md-name (gensym "SUBITEM-2")
                                      :label "subitem2")
                                            (mk-menu-item :label "subitem3")
                                            )))
                             (mk-image-menu-item
                              :stock :harddisk
                              :on-activate (callback (widget event data)
                                             (trc "HARDDISK" widget event data)
                                             (force-output)))
                             (mk-image-menu-item 
                              :image (mk-image :stock :dialog-info :icon-size :menu)
                              :label-widget (mk-label :markup (with-markup (:foreground :blue)
                                                                "Blue label")))
                             (mk-image-menu-item 
                              :stock :my-g
                              :label "user stock icon")
                             ))
                     (mk-menu-item 
                      :md-name 'menu-2
                      :label "Menu 2"
                      :visible (c? (value (fm^ :menu2-visible)))
                      :sensitive (c? (let ((x (fm^ :menu2-sensitive)))
                                       (trc "located m2sensi" x)
                                       (value x)))
                      :kids (kids-list?
                             (mk-tearoff-menu-item)
                             (mk-check-menu-item 
                              :label "Sub-option 1"
                              :accel '(#\a :control)
                              :sensitive (c? (value (fm^ :menu2-option1-sensitive)))
                              :md-name :sub-option1)
                             (mk-separator-menu-item)					  
                             (mk-check-menu-item 
                              :label "Sub-option 2"
                              :md-name :sub-option2
                              :init t))
                      )
                     (mk-menu-item 
                      :label "Menu 3"
                      :md-name :menu3
                      :kids (kids-list? 
                             (mk-radio-menu-item 
                              :md-name :value1 
                              :label "Value 1"
                              :accel '(#\1 :control))
                             (mk-radio-menu-item 
                              :md-name :value2  :init t
                              :label "Value 2"
                              :accel '(#\2 :control))
                             (mk-radio-menu-item 
                              :md-name :value3 
                              :label "Value 3"
                              :accel '(#\3 :control))))))
             (mk-hbox 
              :expand t :fill t
              :kids (kids-list?
                     (mk-toolbar
                      :orientation :vertical
                      :kids (loop for stock-item in '(:justify-center :justify-fill :justify-left :justify-right
                                                       :network :new :no :ok :open :paste :preferences)
                                collect (mk-tool-button 
                                         :stock stock-item
                                         :on-clicked (callback (w e d)
                                                       (setf (value (fm^ :info-label)) (stock self))))))
                     (mk-vbox
                      :expand t :fill t
                      :kids (kids-list?
                             (mk-toolbar 	      
                              :kids (loop for i from 1
                                        for stock-item in '(:remove :revert-to-saved :save :save-as :select-font
                                                             :sort-ascending :sort-descending :spell-check :stop
                                                             :strikethrough :undelete :underline :undo :unindent
                                                             :yes :zoom-100 :zoom-fit :zoom-in :zoom-out)
                                        append (append 
                                                (list (mk-tool-button 
                                                       :stock stock-item
                                                       :on-clicked (callback (w e d)
                                                                     (setf (value (fm^ :info-label)) (stock self)))))
                                                (when (= (mod i 5) 0) (list (mk-separator-tool-item))))))
                             (mk-label
                              :md-name :info-label
                              :visible (c? (value self))
                              :markup (c? (with-markup (:foreground :blue)
                                            "Tool button"
                                            (with-markup (:foreground :red)
                                              (format nil "~a" (value self)))
                                            "clicked")))
                             (mk-hbox
                              :kids (kids-list?
                                     (mk-check-button 
                                      :label "Menu 2 visible" 
                                      :md-name :menu2-visible 
                                      :init t)
                                     (mk-check-button 
                                      :label "Menu 2 sensitive" 
                                      :md-name :menu2-sensitive)
                                     (mk-check-button 
                                      :label "Menu 2 option 1 sensitive" 
                                      :md-name :menu2-option1-sensitive)))
                             (mk-hseparator :padding 5)
                             (mk-hbox
                              :homogeneous t
                              :kids (kids-list? 
                                     (mk-label 
                                      :text (c? (format nil "Menu2 Sub-option 1 : ~a" (value (fm^ :sub-option1)))))
                                     (mk-label 
                                      :text (c? (format nil "Menu2 Sub-option 2 : ~a" (value (fm^ :sub-option2)))))
                                     (mk-label 
                                      :text (c? (format nil "Menu3 value : ~a" (value (fm^ :menu3)))))))
                             (mk-hseparator :padding 5)
                             (mk-hbox
                              :kids (kids-list?
                                     (mk-combo-box
                                      :md-name :combo
                                      :init (c? (third (items self)))
                                      :items (list :item1 :item2 :item3 :item4))
                                     (mk-label 
                                      :text (c? (format nil "Combo value ~a" (value (fm^ :combo)))))))
                             (mk-hseparator :padding 5)
                             (mk-hbox
                              :kids (kids-list?
                                     (mk-combo-box 
                                      :roots '("Text" "Numeric" "Timepoint")
                                      :init '(0)
                                      :children-fn 
                                      #'(lambda (x) 
                                          (cond ((equal x "Text") '("Trimmed Text" "Raw Text"))
                                            ((equal x "Numeric") '("Integer" "Decimal" "Scientific"))
                                            ((equal x "Timepoint")
                                             '("DD/MM/YY" "DD/MM/YYYY" "MM/DD/YY" "YYYY-MM-DD"
                                               "YYYY-MM-DDTHH:MM:SS" "DD/MM/YY HH:MM:SS")))))))
                             (mk-hseparator :padding 5)
                              (mk-hbox
                              :kids (kids-list?
                                     (mk-event-box
                                      :popup (mk-menu
                                              :kids (kids-list?
                                                     (mk-menu-item :label "Test 1")
                                                     (mk-image-menu-item 
                                                      :label "Test image"
                                                      :image (mk-image :stock :cdrom :icon-size :menu))
                                                     (mk-menu-item :label "Test 2")
                                                     (mk-menu-item :label "Test 3")))
                                      :kids (kids-list?
                                             (mk-label
                                              :text "Right click to popup"))))))))))))
