#|

 Gtk ffi

 Copyright (c) 2004 by Vasilis Margioulas <vasilism@sch.gr>

 You have the right to distribute and use this software as governed by 
 the terms of the Lisp Lesser GNU Public License (LLGPL):

    (http://opensource.franz.com/preamble.html)
 
 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 Lisp Lesser GNU Public License for more details.
 
|#

(in-package :gtk-ffi)


(def-gtk-lib-functions :gtk
    ;; main-loop
    (gtk-init :void
	      ((argc :pointer) (argv :pointer)))
  (gtk-init-check gtk-boolean
		  ((argc :pointer)
		   (argv :pointer)))
  (gtk-events-pending gtk-boolean ())
  (gtk-main-iteration gtk-boolean ())
  (gtk-main-iteration-do gtk-boolean
			 ((blocking gtk-boolean)))
  (gtk-main :void ())
  (gtk-main-quit :void ())
  (gtk-quit-add :unsigned-int
		((main-level :unsigned-int)
		 (function :pointer)
		 (data :pointer)))
  (gtk-quit-remove :void
		   ((quit-handler-id :unsigned-int)))
  (gtk-main-level :int ())
  (gtk-get-current-event-time :unsigned-int ())

  ;; misc
  (gtk-misc-set-alignment :void
			  ((widget :pointer)
			   (xalign :float)
			   (yalign :float)))
  (gtk-misc-set-padding :void
			((widget :pointer)
			 (xpad :float)
			 (ypad :float)))
  ;;container
  (gtk-container-add :pointer
		     ((container :pointer)
		      (widget :pointer)))
  (gtk-container-remove :void
			((container :pointer)
			 (widget :pointer)))
  (gtk-container-set-border-width :void
				  ((container :pointer)
				   (width :unsigned-int)))
  (gtk-container-get-border-width :unsigned-int
				  ((container :pointer)))
  (gtk-container-set-resize-mode :void
				 ((container :pointer)
				  (mode :unsigned-int)))
  (gtk-container-get-resize-mode :unsigned-int
				 ((container :pointer)))
  ;;box
  (gtk-box-pack-start :void
		      ((box :pointer)
		       (widget :pointer)
		       (expand gtk-boolean)
		       (fill gtk-boolean)
		       (padding :int)))
  (gtk-box-pack-start-defaults :void
			       ((box :pointer)
				(widget
				 :pointer)))
  (gtk-box-set-homogeneous :void
			   ((box :pointer)
			    (homogeneous
			     gtk-boolean)))
  (gtk-box-set-spacing :void
		       ((box :pointer)
			(spacing :int)))
  (gtk-hbox-new :pointer
		((homogeneous gtk-boolean)
		 (spacing :int)))
  (gtk-vbox-new :pointer
		((homogeneous gtk-boolean)
		 (spacing :int)))
  (gtk-drawing-area-new :pointer ())

  ;;table
  (gtk-table-new :pointer
		 ((rows :unsigned-int)
		  (columns :unsigned-int)
		  (homogeneous gtk-boolean)))
  (gtk-table-attach :void
		    ((table :pointer)
		     (child :pointer)
		     (l-attach :unsigned-int)
		     (r-attach :unsigned-int)
		     (t-attach :unsigned-int)
		     (b-attach :unsigned-int)
		     (x-options :int)
		     (y-options :int)
		     (x-padding :int)
		     (y-padding :int)))
  (gtk-table-attach-defaults :void
			     ((table :pointer)
			      (child :pointer)
			      (l-attach
			       :unsigned-int)
			      (r-attach
			       :unsigned-int)
			      (t-attach
			       :unsigned-int)
			      (b-attach
			       :unsigned-int)))
  (gtk-table-set-homogeneous :void
			     ((table :pointer)
			      (homogeneous
			       gtk-boolean)))

  ;;paned
  (gtk-paned-add1 :void
		  ((paned :pointer)
		   (child :pointer)))
  (gtk-paned-add2 :void
		  ((paned :pointer)
		   (child :pointer)))
  (gtk-paned-set-position :void
			  ((paned :pointer)
			   (pos :int)))
  (gtk-hpaned-new :pointer ())
  (gtk-vpaned-new :pointer ())

  ;;expander
  (gtk-expander-new :pointer
		    ((label gtk-string)))
  (gtk-expander-set-expanded :void
			     ((expander :pointer)
			      (expanded
			       gtk-boolean)))
  (gtk-expander-set-spacing :void
			    ((expander :pointer)
			     (spacing :pointer)))
  (gtk-expander-set-label :void
			  ((expander :pointer)
			   (label gtk-string)))
  (gtk-expander-set-use-underline :void
				  ((expander
				    :pointer)
				   (use-underline
				    gtk-boolean)))
  (gtk-expander-set-use-markup :void
			       ((expander
				 :pointer)
				(use-markup
				 gtk-boolean)))
  (gtk-expander-set-label-widget :void
				 ((expander
				   :pointer)
				  (label-widget
				   :pointer)))

  ;;alignment
  (gtk-alignment-new :pointer
		     ((xalign :float)
		      (yalign :float)
		      (xscale :float)
		      (yscale :float)))
  (gtk-alignment-set :void
		     ((alignment :pointer)
		      (xalign :float)
		      (yalign :float)
		      (xscale :float)
		      (yscale :float)))

  ;;frame
  (gtk-frame-new :pointer ((label gtk-string)))
  (gtk-frame-set-label :void
		       ((frame :pointer)
			(label gtk-string)))
  (gtk-frame-set-label-widget :void
			      ((frame :pointer)
			       (label-widget
				:pointer)))
  (gtk-frame-set-label-align :void
			     ((frame :pointer)
			      (xalign :float)
			      (yalign :float)))
  (gtk-frame-set-shadow-type :void
			     ((frame :pointer)
			      (shadow-type :int)))

  ;;aspect-frame
  (gtk-aspect-frame-new :pointer
			((label gtk-string)
			 (xalign :float)
			 (yalign :float)
			 (ratio :float)
			 (obey_child
			  gtk-boolean)))
  ;; separator
  (gtk-hseparator-new :pointer ())
  (gtk-vseparator-new :pointer ())

  ;;scrolled
  (gtk-scrolled-window-new :pointer
			   ((hadjustment :pointer)
			    (vadjustment
			     :pointer)))
  (gtk-scrolled-window-set-policy :void
				  ((scrolled-window
				    :pointer)
				   (h-policy :int)
				   (v-policy :int)))
  (gtk-scrolled-window-add-with-viewport :void
					 ((scrolled-window :pointer)
					  (child :pointer)))
  (gtk-scrolled-window-set-placement :void
				     ((scrolled-window :pointer)
				      (placement :int)))
  (gtk-scrolled-window-set-shadow-type :void
				       ((scrolled-window :pointer)
					(type :int)))

  ;;notebook
  (gtk-notebook-new :pointer ())
  (gtk-notebook-append-page :int
			    ((notebook :pointer)
			     (child :pointer)
			     (tab-label :pointer)))
  (gtk-notebook-append-page-menu :int
				 ((notebook :pointer)
				  (child :pointer)
				  (tab-label :pointer)
				  (menu-label :pointer)))
  (gtk-notebook-prepend-page :int
			     ((notebook :pointer)
			      (child :pointer)
			      (tab-label :pointer)))
  (gtk-notebook-prepend-page-menu :int
				  ((notebook :pointer)
				   (child :pointer)
				   (tab-label :pointer)
				   (menu-label :pointer)))
  (gtk-notebook-insert-page :int
			    ((notebook :pointer)
			     (child :pointer)
			     (tab-label :pointer)
			     (pos :int)))
  (gtk-notebook-insert-page-menu :int
				 ((notebook :pointer)
				  (child :pointer)
				  (tab-label :pointer)
				  (menu-label :pointer)
				  (pos :int)))
  (gtk-notebook-remove-page :void
			    ((notebook :pointer)
			     (page-num :int)))
  (gtk-notebook-set-current-page :void
				 ((notebook :pointer)
				  (page-num :int)))
  (gtk-notebook-get-current-page :int
				 ((notebook :pointer)))
  (gtk-notebook-set-tab-pos :void
			    ((notebook :pointer)
			     (pos :int)))
  (gtk-notebook-set-show-tabs :void
			      ((notebook :pointer)
			       (show-tabs gtk-boolean)))
  (gtk-notebook-set-show-border :void
				((notebook :pointer)
				 (show-border gtk-boolean)))
  (gtk-notebook-set-scrollable :void
			       ((notebook :pointer)
				(scrollable gtk-boolean)))
  (gtk-notebook-set-tab-border :void
			       ((notebook :pointer)
				(border-width :int)))
  (gtk-notebook-popup-enable :void
			     ((notebook :pointer)))
  (gtk-notebook-popup-disable :void
			      ((notebook :pointer)))
  (gtk-notebook-set-homogeneous-tabs :void
				     ((notebook :pointer)
				      (homogeneous-tabs gtk-boolean)))
  (gtk-notebook-get-nth-page :pointer
			     ((notebook :pointer)
			      (n :int)))
  (gtk-notebook-get-n-pages :int
			    ((notebook :pointer)))

  ;;label
  (gtk-label-new :pointer ((text gtk-string)))
  (gtk-label-set-text :void
		      ((label :pointer)
		       (text gtk-string)))
  (gtk-label-set-text-with-mnemonic :void
				    ((label :pointer)
				     (text gtk-string)))
  (gtk-label-set-line-wrap :void
			   ((label :pointer)
			    (wrap gtk-boolean)))
  (gtk-label-set-selectable :void
			    ((label :pointer)
			     (selectable gtk-boolean)))
  (gtk-label-set-use-markup :void
			    ((label :pointer)
			     (use-markup gtk-boolean)))
  (gtk-label-set-markup :void
			((label :pointer)
			 (markup gtk-string)))
  (gtk-label-set-markup-with-mnemonic :void
				      ((label :pointer)
				       (markup gtk-string)))
  (gtk-accel-label-new :pointer
		       ((str gtk-string)))
  (gtk-accel-label-set-accel-widget :void
				    ((label :pointer)
				     (widget :pointer)))

  ;;progress
  (gtk-progress-bar-new :pointer ())
  (gtk-progress-bar-pulse :void ((pbar :pointer)))
  (gtk-progress-bar-set-text :void
			     ((pbar :pointer)
			      (text gtk-string)))
  (gtk-progress-bar-set-fraction :void
				 ((pbar :pointer)
				  (fraction :double)))
  (gtk-progress-bar-set-pulse-step :void
				   ((pbar :pointer)
				    (fraction :double)))
  (gtk-progress-bar-set-orientation :void
				    ((pbar :pointer)
				     (orientation :int)))
  (gtk-progress-bar-set-bar-style :void
				  ((pbar :pointer)
				   (style :int)))
  (gtk-progress-bar-set-discrete-blocks :void
					((pbar :pointer)
					 (blocks :unsigned-int)))
  (gtk-progress-bar-set-activity-step :void
				      ((pbar :pointer)
				       (step :unsigned-int)))
  (gtk-progress-bar-set-activity-blocks :void
					((pbar :pointer)
					 (blocks :unsigned-int)))
  (gtk-progress-bar-update :void
			   ((pbar :pointer)
			    (percentage :double)))

  ;;image
  (gtk-image-new-from-file :pointer
			   ((filename gtk-string)))
  (gtk-image-new-from-stock :pointer
			    ((stock gtk-string)
			     (icon-size :int)))
  (gtk-image-set-from-stock :void
			    ((image :pointer)
			     (stock gtk-string)
			     (icon-size :int)))
  (gtk-image-get-pixbuf :pointer
			((image :pointer)))

  ;;icon-factory
  (gtk-icon-factory-new :pointer ())
  (gtk-icon-factory-add-default :void
				((factory :pointer)))
  (gtk-icon-factory-add :void
			((factory :pointer)
			 (stock-id gtk-string)
			 (icon-set :pointer)))

  ;;icon-set
  (gtk-icon-set-new-from-pixbuf :pointer
				((pixbuf :pointer)))

  ;;statusbar
  (gtk-statusbar-new :pointer ())
  (gtk-statusbar-get-context-id :unsigned-int
				((sbar :pointer)
				 (description gtk-string)))
  (gtk-statusbar-push :unsigned-int
		      ((sbar :pointer)
		       (context-id :unsigned-int)
		       (text gtk-string)))
  (gtk-statusbar-pop :void
		     ((sbar :pointer)
		      (context-id :unsigned-int)))
  (gtk-statusbar-remove :void
			((sbar :pointer)
			 (context-id :unsigned-int)
			 (message-id :unsigned-int)))
  (gtk-statusbar-set-has-resize-grip :void
				     ((sbar :pointer)
				      (setting gtk-boolean)))

  ;;widget
  (gtk-widget-show :void ((widget :pointer)))
  (gtk-widget-show-all :void ((widget :pointer)))
  (gtk-widget-hide :void ((widget :pointer)))
  (gtk-widget-hide-all :void ((widget :pointer)))
  (gtk-widget-hide-on-delete :void
			     ((widget :pointer)))
  (gtk-widget-destroy :void ((widget :pointer)))
  (gtk-widget-set-sensitive :void
			    ((widget :pointer)
			     (sensitive gtk-boolean)))
  (gtk-widget-set-size-request :void
			       ((widget :pointer)
				(width :int)
				(height :int)))
  (gtk-widget-get-parent-window :pointer
				((widget :pointer)))
  (gtk-widget-add-accelerator :void
			      ((widget :pointer)
			       (gsignal gtk-string)
			       (accel-group :pointer)
			       (key :unsigned-int)
			       (mods :int)
			       (flags :int)))
  (gtk-widget-grab-focus :void
			 ((widget :pointer)))
  (gtk-widget-add-events :void
			 ((widget :pointer)
			  (events :int)))
  (gtk-widget-set-events :void
			 ((widget :pointer)
			  (events :int)))
  (gtk-widget-get-events :int ((widget :pointer)))
  (gtk-widget-queue-draw :void ((widget :pointer)))

  ;;window
  (gtk-window-new :pointer ((type :int)))
  (gtk-window-set-title :void
			((widget :pointer)
			 (title gtk-string)))
  (gtk-window-set-icon-from-file gtk-boolean
				 ((window :pointer)
				  (filename gtk-string)
				  (err :pointer)))
  (gtk-window-set-default-size :void
			       ((widget :pointer)
				(width :int)
				(height :int)))
  (gtk-window-set-resizable :void
			    ((widget :pointer)
			     (resizable gtk-boolean)))
  (gtk-window-set-decorated :void
			    ((widget :pointer)
			     (decorated gtk-boolean)))
  (gtk-window-set-auto-startup-notification :void
					    ((setting
					      gtk-boolean)))
  (gtk-window-set-position :void
			   ((widget :pointer)
			    (position :int)))
  (gtk-window-maximize :void ((widget :pointer)))
  (gtk-window-unmaximize :void
			 ((widget :pointer)))
  (gtk-window-iconify :void ((widget :pointer)))
  (gtk-window-deiconify :void ((widget :pointer)))
  (gtk-window-fullscreen :void
			 ((widget :pointer)))
  (gtk-window-unfullscreen :void
			   ((widget :pointer)))
  (gtk-window-add-accel-group :void
			      ((window :pointer)
			       (accel-group :pointer)))

  ;;entry
  (gtk-entry-new :pointer ())
  (gtk-entry-set-text :void
		      ((entry :pointer)
		       (text gtk-string)))
  (gtk-entry-get-text :pointer ((entry :pointer)))
  (gtk-entry-set-max-length :void
			    ((entry :pointer)
			     (max-length :int)))
  (gtk-entry-set-editable :void
			  ((entry :pointer)
			   (editable
			    gtk-boolean)))
  (gtk-entry-set-completion :void
			    ((entry :pointer)
			     (completion :pointer)))
  (gtk-entry-set-has-frame :void
			   ((entry :pointer)
			    (has-frame gtk-boolean)))

  ;;entry-completion
  (gtk-entry-completion-new :pointer ())
  (gtk-entry-completion-set-model :void
				  ((completion :pointer)
				   (model :pointer)))
  (gtk-entry-completion-set-text-column :void
					((completion :pointer)
					 (column :int)))

  ;;range
  (gtk-range-set-range :void
		       ((range :pointer)
			(minval :double)
			(maxval :double)))
  (gtk-range-set-value :void
		       ((range :pointer)
			(val :double)))
  (gtk-range-set-inverted :void
			  ((range :pointer)
			   (inverted
			    gtk-boolean)))
  (gtk-range-set-increments :void
			    ((range :pointer)
			     (step :double)
			     (page :double)))
  (gtk-range-set-update-policy :void
			       ((range :pointer)
				(policy :int)))
  (gtk-range-get-value :double ((range :pointer)))

  ;;scale
  (gtk-scale-set-draw-value :void
			    ((scale :pointer)
			     (draw-value gtk-boolean)))
  (gtk-scale-set-value-pos :void
			   ((scale :pointer)
			    (pos-type :int)))
  (gtk-scale-set-digits :void
			((scale :pointer)
			 (digits :int)))

  ;;hscale
  (gtk-hscale-new :pointer
		  ((adjustment :pointer)))
  (gtk-hscale-new-with-range :pointer
			     ((minval :double)
			      (maxval :double)
			      (step :double)))

  ;;vscale
  (gtk-vscale-new :pointer
		  ((adjustment :pointer)))
  (gtk-vscale-new-with-range :pointer
			     ((minval :double)
			      (maxval :double)
			      (step :double)))

  ;;combo-box
  (gtk-combo-box-new-text :pointer ())
  (gtk-combo-box-append-text :void
			     ((combo-box :pointer)
			      (text gtk-string)))
  (gtk-combo-box-remove-text :void
			     ((combo-box :pointer)
			      (position :int)))
  (gtk-combo-box-set-active :void
			    ((combo-box :pointer)
			     (index :int)))
  (gtk-combo-box-get-active :int
			    ((combo-box :pointer)))
  (gtk-combo-box-set-model :void
			   ((combo-box :pointer)
			    (model :pointer)))
  (gtk-combo-box-get-active-iter gtk-boolean
				 ((combo-box :pointer)
				  (iter :pointer)))
  (gtk-combo-box-set-active-iter :void
				 ((combo-box :pointer)
				  (iter :pointer)))

  ;;combo-box-entry
  (gtk-combo-box-entry-new :pointer ())
  (gtk-combo-box-entry-new-text :pointer ())
  (gtk-combo-box-entry-new-with-model :pointer ((tree-model :pointer)
                                                (text-column :int)))
  (gtk-combo-box-entry-get-text-column :int
                                       ((combo-box-entry :pointer))) 
  (gtk-combo-box-entry-set-text-column :void
                                       ((combo-box-entry :pointer)
                                        (column :int)))

  ;;calendar
  (gtk-calendar-new :pointer ())
  (gtk-calendar-get-date :void
			 ((cal :pointer)
			  (year :pointer)
			  (month :pointer)
			  (day :pointer)))
  (gtk-calendar-select-month :int
			     ((cal :pointer)
			      (month :unsigned-int)
			      (year :unsigned-int)))
  (gtk-calendar-select-day :void
			   ((cal :pointer)
			    (day :unsigned-int)))

  ;;arrow
  (gtk-arrow-new :pointer
		 ((arrow-type :int)
		  (shadow-type :int)))
  (gtk-arrow-set :void
		 ((arrow :pointer)
		  (arrow-type :int)
		  (shadow-type :int)))

  ;;dialog
  (gtk-dialog-new :pointer ())
  (gtk-dialog-run :int ((dialog :pointer)))
  (gtk-dialog-response :void
		       ((dialog :pointer)
			(response-id :int)))
  (gtk-dialog-add-button :pointer
			 ((dialog :pointer)
			  (button-text
			   gtk-string)
			  (response-id :int)))
  (gtk-dialog-add-action-widget :void
				((dialog :pointer)
				 (child :pointer)
				 (response-id :pointer)))
  (gtk-dialog-set-has-separator :void
				((dialog :pointer)
				 (has-separator gtk-boolean)))
  (gtk-dialog-set-default-response :void
				   ((dialog :pointer)
				    (response-id :int)))

  ;;message-dialog
  (gtk-message-dialog-new :pointer
			  ((parent :pointer)
			   (flags :int)
			   (type :int)
			   (buttons :int)
			   (message gtk-string)))
  (gtk-message-dialog-set-markup :void
				 ((dialog
				   :pointer)
				  (str gtk-string)))

  ;;file-chooser
  (gtk-file-chooser-set-action :void
			       ((chooser :pointer)
				(action :int)))
  (gtk-file-chooser-set-local-only :void
				   ((chooser :pointer)
				    (local-only gtk-boolean)))
  (gtk-file-chooser-set-select-multiple :void
					((chooser :pointer)
					 (select-multiple gtk-boolean)))
  (gtk-file-chooser-set-current-name :void 
				     ((chooser :pointer)
				      (name gtk-string)))
  (gtk-file-chooser-set-filename gtk-boolean 
				 ((chooser :pointer)
				  (filename gtk-string)))
  (gtk-file-chooser-get-filename gtk-string
				 ((chooser :pointer)))
  (gtk-file-chooser-get-filenames :pointer
				  ((chooser :pointer)))
  (gtk-file-chooser-set-current-folder gtk-boolean
				       ((chooser :pointer)
					(folder gtk-string)))
  (gtk-file-chooser-get-current-folder gtk-string
				       ((chooser :pointer)))
  (gtk-file-chooser-set-uri gtk-boolean
			    ((chooser :pointer)
			     (uri gtk-string)))
  (gtk-file-chooser-get-uri gtk-string
			    ((chooser :pointer)))
  (gtk-file-chooser-select-uri gtk-boolean
			       ((chooser :pointer)))
  (gtk-file-chooser-get-uris :pointer
			     ((chooser :pointer)))
  (gtk-file-chooser-set-current-folder-uri gtk-boolean
					   ((chooser :pointer)
					    (folder gtk-string)))
  (gtk-file-chooser-get-current-folder-uri gtk-string
					   ((chooser :pointer)))
  (gtk-file-chooser-set-use-preview-label :void
					  ((chooser :pointer)
					   (use-label gtk-boolean)))
  (gtk-file-chooser-add-filter :void
			       ((chooser :pointer)
				(filter :pointer)))
  (gtk-file-chooser-set-filter :void
			       ((chooser :pointer)
				(filter :pointer)))

  ;;file-chooser-widget
  (gtk-file-chooser-widget-new :pointer
			       ((action :int)))

  ;;file-chooser-dialog
  (gtk-file-chooser-dialog-new :pointer
			       ((title gtk-string)
				(parent :pointer)
				(action :int)
				(cancel-text gtk-string)
				(cancel-response-id :int)
				(accept-text gtk-string)
				(accept-response-id :int)
				(null :pointer)))

  ;;file-filter
  (gtk-file-filter-new :pointer ())
  (gtk-file-filter-set-name :void
			    ((filter :pointer)
			     (name gtk-string)))
  (gtk-file-filter-add-mime-type :void
				 ((filter :pointer)
				  (mime-type gtk-string)))
  (gtk-file-filter-add-pattern :void
			       ((filter :pointer)
				(pattern gtk-string)))
  ;;text-view
  (gtk-text-view-new :pointer ())
  (gtk-text-view-set-buffer :void
			    ((text-view :pointer)
			     (buffer :pointer)))
  (gtk-text-view-get-buffer :pointer
			    ((text-view :pointer)))
  (gtk-text-view-set-wrap-mode :void
			       ((text-view :pointer)
				(wrap-mode :int)))
  (gtk-text-view-set-editable :void
			      ((text-view :pointer)
			       (setting gtk-boolean)))
  (gtk-text-view-scroll-to-iter gtk-boolean
				((text-view :pointer)
				 (iter :pointer)
				 (within-margin :double)
				 (use-align gtk-boolean)
				 (xalign :double)
				 (yalign :double)))
  (gtk-text-view-scroll-mark-onscreen :void
				      ((text-view :pointer)
				       (mark :pointer)))
  ;;text-buffer
  (gtk-text-buffer-new :pointer
		       ((table :pointer)))
  (gtk-text-buffer-get-text gtk-string
			    ((buffer :pointer)
			     (start :pointer)
			     (end :pointer)
			     (include-hidden-chars gtk-boolean)))
  (gtk-text-buffer-insert :void
			  ((buffer :pointer)
			   (iter :pointer)
			   (text gtk-string)
			   (len :int)))
  (gtk-text-buffer-delete :void
			  ((buffer :pointer)
			   (start :pointer)
			   (end :pointer)))
  (gtk-text-buffer-set-text :void
			    ((buffer :pointer)
			     (text gtk-string)
			     (len :int)))
  (gtk-text-buffer-get-char-count :int
				  ((buffer :pointer)))
  (gtk-text-buffer-insert-at-cursor :void
				    ((buffer :pointer)
				     (text :pointer)
				     (len :int)))
  (gtk-text-buffer-create-tag :pointer
			      ((buffer :pointer)
			       (tag-name gtk-string)
			       (first-property-name gtk-string)
			       (second-property-name gtk-string)
			       (eof :pointer)))
  (gtk-text-buffer-remove-all-tags :void
				   ((buffer :pointer)
				    (start :pointer)
				    (end :pointer)))
  (gtk-text-buffer-get-iter-at-offset :void
				      ((buffer :pointer)
				       (iter :pointer)
				       (char-offset :int)))
  (gtk-text-buffer-get-end-iter :void
				((buffer :pointer)
				 (iter :pointer)))
  (gtk-text-buffer-apply-tag :void
			     ((buffer :pointer)
			      (tag :pointer)
			      (start :pointer)
			      (end :pointer)))
  (gtk-text-buffer-create-mark :pointer
			       ((buffer :pointer)
				(mark-name gtk-string)
				(where :pointer)
				(left-gravity gtk-boolean)))
  (gtk-text-buffer-get-iter-at-mark :void
				    ((buffer :pointer)
				     (iter :pointer)
				     (mark :pointer)))
  (gtk-text-buffer-get-insert :pointer
			      ((buffer :pointer)))
  (gtk-text-buffer-delete-mark :void
			       ((buffer :pointer)
				(mark :pointer)))
  (gtk-text-buffer-get-mark :pointer
			    ((buffer :pointer)
			     (name gtk-string)))
  (gtk-text-buffer-get-modified gtk-boolean
				((buffer :pointer)))
  (gtk-text-buffer-set-modified :void
				((buffer :pointer)
				 (setting gtk-boolean)))
  (gtk-text-buffer-move-mark :void
			     ((buffer :pointer)
			      (mark :pointer)
			      (where :pointer)))
  ;;text iter
  (gtk-text-iter-free :void ((iter :pointer)))
  (gtk-text-iter-get-offset :int
			    ((iter :pointer)))

  ;;text-tag-table
  (gtk-text-tag-table-new :pointer ())
  (gtk-accel-group-new :pointer ())

  ;;ui-manager
  (gtk-ui-manager-new :pointer ())
  (gtk-ui-manager-set-add-tearoffs :void
				   ((ui-manager :pointer)
				    (add-tearoffs gtk-boolean)))
  (gtk-ui-manager-insert-action-group :void
				      ((ui-manager :pointer)
				       (action-group :pointer)
				       (pos :int)))
  (gtk-ui-manager-get-toplevels :pointer
				((ui-manager :pointer)
				 (types :int)))

  ;;action-group
  (gtk-action-group-new :pointer
			((name gtk-string)))
  (gtk-action-group-set-sensitive :void
				  ((action-group :pointer)
				   (sensitive gtk-boolean)))
  (gtk-action-group-set-visible :void
				((action-group :pointer)
				 (visible gtk-boolean)))
  (gtk-action-group-add-action :void
			       ((action-group :pointer)
				(action :pointer)))
  (gtk-action-group-remove-action :void
				  ((action-group :pointer)
				   (action :pointer)))
  (gtk-action-group-add-action-with-accel :void
					  ((action-group :pointer)
					   (action :pointer)
					   (accel gtk-string)))

  ;;action
  (gtk-action-new :pointer
		  ((name gtk-string)
		   (label :pointer)
		   (tooltip :pointer)
		   (stock-id gtk-string)))
  (gtk-event-box-new :pointer ())
  (gtk-event-box-set-above-child :void
				 ((event-box :pointer)
				  (above gtk-boolean)))
  (gtk-event-box-set-visible-window :void
				    ((event-box :pointer)
				     (visible-window gtk-boolean)))

  ;;miscellaneous
  (gtk-check-version gtk-string
		     ((required-major :unsigned-int)
		      (required-minor :unsigned-int)
		      (required-micro :unsigned-int)))
  (gtk-widget-create-pango-layout :pointer
				  ((widget :pointer)
				   (text gtk-string))))
;;; t3
#+libcellsgtk
(def-gtk-lib-functions :cgtk
  (gtk-adds-dialog-vbox :pointer
			((dialog :pointer)))
  (gtk-adds-text-view-popup-menu :pointer
				 ((treeview :pointer)))
  (gtk-adds-text-iter-new :pointer ())
  (gtk-adds-tree-iter-new :pointer ())
  (gtk-adds-widget-visible-p :int
			     ((widget :pointer)))
  (gtk-adds-widget-mapped-p :int
			    ((widget :pointer)))
  (gtk-adds-widget-window :pointer
			  ((widget :pointer)))
  (gtk-adds-color-new :pointer ())
  (gtk-adds-color-set-rgb :void
			  ((gdkcolor :pointer)
			   (red :unsigned-int)
			   (green :unsigned-int)
			   (blue :unsigned-int)))
  (gtk-adds-ok :int ())
  (gtk-adds-g-thread-supported :int ())
  (gtk-adds-widget-width :int ((widget :pointer)))
  (gtk-adds-widget-height :int ((widget :pointer))))


#-libcellsgtk
(defmacro you-need-libcellsgtk (&body names)
  `(progn
     ,@(loop for name in names
           collect `(progn
                      (export '(,name))
                      (defun ,name (&rest ignore) 
                        (declare (ignore ignore))
                        (error "You need libcellsgtk.so to use function ~A." ',name))))))

#-libcellsgtk
(eval-when (:compile-toplevel :load-toplevel :execute)
  (you-need-libcellsgtk
   gtk-adds-dialog-vbox
   gtk-adds-text-view-popup-menu
   gtk-adds-text-iter-new
   gtk-adds-tree-iter-new
   gtk-adds-widget-mapped-p
   gtk-adds-widget-visible-p
   gtk-adds-widget-window
   gtk-adds-color-new
   gtk-adds-color-set-rgb))
