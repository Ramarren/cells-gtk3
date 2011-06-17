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
  (gtk-init-check gboolean
		  ((argc :pointer)
		   (argv :pointer)))
  (gtk-events-pending gboolean ())
  (gtk-main-iteration gboolean ())
  (gtk-main-iteration-do gboolean
			 ((blocking gboolean)))
  (gtk-main :void ())
  (gtk-main-quit :void ())
  (gtk-quit-add guint
		((main-level guint)
		 (function :pointer)
		 (data gpointer)))
  (gtk-quit-remove :void
		   ((quit-handler-id guint)))
  (gtk-main-level guint ())
  (gtk-get-current-event-time guint32 ())

  ;; misc
  (gtk-misc-set-alignment :void
			  ((widget :pointer)
			   (xalign gfloat)
			   (yalign gfloat)))
  (gtk-misc-set-padding :void
			((widget :pointer)
			 (xpad gint)
			 (ypad gint)))
  ;;container
  (gtk-container-add :void
		     ((container :pointer)
		      (widget :pointer)))
  (gtk-container-remove :void
			((container :pointer)
			 (widget :pointer)))
  (gtk-container-set-border-width :void
				  ((container :pointer)
				   (width guint)))
  (gtk-container-get-border-width guint
				  ((container :pointer)))
  (gtk-container-set-resize-mode :void
				 ((container :pointer)
				  (mode gtk-resize-mode)))
  (gtk-container-get-resize-mode gtk-resize-mode
				 ((container :pointer)))
  ;;bin
  (gtk-bin-get-child :pointer ((bin :pointer)))
  ;;box
  (gtk-box-pack-start :void
		      ((box :pointer)
		       (widget :pointer)
		       (expand gboolean)
		       (fill gboolean)
		       (padding guint)))
  (gtk-box-pack-start-defaults :void
			       ((box :pointer)
				(widget :pointer)))
  (gtk-box-set-homogeneous :void
			   ((box :pointer)
			    (homogeneous gboolean)))
  (gtk-box-set-spacing :void
		       ((box :pointer)
			(spacing gint)))
  (gtk-hbox-new :pointer
		((homogeneous gboolean)
		 (spacing gint)))
  (gtk-vbox-new :pointer
		((homogeneous gboolean)
		 (spacing gint)))
  (gtk-drawing-area-new :pointer ())

  ;;table
  (gtk-table-new :pointer
		 ((rows guint)
		  (columns guint)
		  (homogeneous gboolean)))
  (gtk-table-attach :void
		    ((table :pointer)
		     (child :pointer)
		     (l-attach guint)
		     (r-attach guint)
		     (t-attach guint)
		     (b-attach guint)
		     (x-options gtk-attach-options)
		     (y-options gtk-attach-options)
		     (x-padding guint)
		     (y-padding guint)))
  (gtk-table-attach-defaults :void
			     ((table :pointer)
			      (child :pointer)
			      (l-attach guint)
			      (r-attach guint)
			      (t-attach guint)
			      (b-attach guint)))
  (gtk-table-set-homogeneous :void
			     ((table :pointer)
			      (homogeneous gboolean)))
  ;;paned
  (gtk-paned-add1 :void
		  ((paned :pointer)
		   (child :pointer)))
  (gtk-paned-add2 :void
		  ((paned :pointer)
		   (child :pointer)))
  (gtk-paned-set-position :void
			  ((paned :pointer)
			   (pos gint)))
  (gtk-hpaned-new :pointer ())
  (gtk-vpaned-new :pointer ())

  ;;expander
  (gtk-expander-new :pointer
		    ((label gtk-string)))
  (gtk-expander-set-expanded :void
			     ((expander :pointer)
			      (expanded gboolean)))
  (gtk-expander-set-spacing :void
			    ((expander :pointer)
			     (spacing gint)))
  (gtk-expander-set-label :void
			  ((expander :pointer)
			   (label gtk-string)))
  (gtk-expander-set-use-underline :void
				  ((expander :pointer)
				   (use-underline gboolean)))
  (gtk-expander-set-use-markup :void
			       ((expander :pointer)
				(use-markup gboolean)))
  (gtk-expander-set-label-widget :void
				 ((expander :pointer)
				  (label-widget :pointer)))

  ;;alignment
  (gtk-alignment-new :pointer
		     ((xalign gfloat)
		      (yalign gfloat)
		      (xscale gfloat)
		      (yscale gfloat)))
  (gtk-alignment-set :void
		     ((alignment :pointer)
		      (xalign gfloat)
		      (yalign gfloat)
		      (xscale gfloat)
		      (yscale gfloat)))

  ;;frame
  (gtk-frame-new :pointer ((label gtk-string)))
  (gtk-frame-set-label :void
		       ((frame :pointer)
			(label gtk-string)))
  (gtk-frame-set-label-widget :void
			      ((frame :pointer)
			       (label-widget :pointer)))
  (gtk-frame-set-label-align :void
			     ((frame :pointer)
			      (xalign gfloat)
			      (yalign gfloat)))
  (gtk-frame-set-shadow-type :void
			     ((frame :pointer)
			      (shadow-type gtk-shadow-type)))

  ;;aspect-frame
  (gtk-aspect-frame-new :pointer
			((label gtk-string)
			 (xalign gfloat)
			 (yalign gfloat)
			 (ratio gfloat)
			 (obey_child
			  gboolean)))
  ;; separator
  (gtk-hseparator-new :pointer ())
  (gtk-vseparator-new :pointer ())

  ;;scrolled
  (gtk-scrolled-window-new :pointer
			   ((hadjustment :pointer)
			    (vadjustment :pointer)))
  (gtk-scrolled-window-set-policy :void
				  ((scrolled-window :pointer)
				   (h-policy gtk-policy-type)
				   (v-policy gtk-policy-type)))
  (gtk-scrolled-window-add-with-viewport :void
					 ((scrolled-window :pointer)
					  (child :pointer)))
  (gtk-scrolled-window-set-placement :void
				     ((scrolled-window :pointer)
				      (placement gtk-corner-type)))
  (gtk-scrolled-window-set-shadow-type :void
				       ((scrolled-window :pointer)
					(type gtk-shadow-type)))

  ;;notebook
  (gtk-notebook-new :pointer ())
  (gtk-notebook-append-page gint
			    ((notebook :pointer)
			     (child :pointer)
			     (tab-label :pointer)))
  (gtk-notebook-append-page-menu gint
				 ((notebook :pointer)
				  (child :pointer)
				  (tab-label :pointer)
				  (menu-label :pointer)))
  (gtk-notebook-prepend-page gint
			     ((notebook :pointer)
			      (child :pointer)
			      (tab-label :pointer)))
  (gtk-notebook-prepend-page-menu gint
				  ((notebook :pointer)
				   (child :pointer)
				   (tab-label :pointer)
				   (menu-label :pointer)))
  (gtk-notebook-insert-page gint
			    ((notebook :pointer)
			     (child :pointer)
			     (tab-label :pointer)
			     (pos gint)))
  (gtk-notebook-insert-page-menu gint
				 ((notebook :pointer)
				  (child :pointer)
				  (tab-label :pointer)
				  (menu-label :pointer)
				  (pos gint)))
  (gtk-notebook-remove-page :void
			    ((notebook :pointer)
			     (page-num gint)))
  (gtk-notebook-set-current-page :void
				 ((notebook :pointer)
				  (page-num gint)))
  (gtk-notebook-get-current-page gint
				 ((notebook :pointer)))
  (gtk-notebook-set-tab-pos :void
			    ((notebook :pointer)
			     (pos gtk-position-type)))
  (gtk-notebook-set-show-tabs :void
			      ((notebook :pointer)
			       (show-tabs gboolean)))
  (gtk-notebook-set-show-border :void
				((notebook :pointer)
				 (show-border gboolean)))
  (gtk-notebook-set-scrollable :void
			       ((notebook :pointer)
				(scrollable gboolean)))
  (gtk-notebook-set-tab-border :void
			       ((notebook :pointer)
				(border-width guint)))
  (gtk-notebook-popup-enable :void
			     ((notebook :pointer)))
  (gtk-notebook-popup-disable :void
			      ((notebook :pointer)))
  (gtk-notebook-set-homogeneous-tabs :void
				     ((notebook :pointer)
				      (homogeneous-tabs gboolean)))
  (gtk-notebook-get-nth-page :pointer
			     ((notebook :pointer)
			      (n gint)))
  (gtk-notebook-get-n-pages gint
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
			    (wrap gboolean)))
  (gtk-label-set-selectable :void
			    ((label :pointer)
			     (selectable gboolean)))
  (gtk-label-set-use-markup :void
			    ((label :pointer)
			     (use-markup gboolean)))
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
				  (fraction gdouble)))
  (gtk-progress-bar-set-pulse-step :void
				   ((pbar :pointer)
				    (fraction gdouble)))
  (gtk-progress-bar-set-orientation :void
				    ((pbar :pointer)
				     (orientation gtk-progress-bar-orientation)))
  (gtk-progress-bar-set-bar-style :void
				  ((pbar :pointer)
				   (style gtk-progress-bar-style)))
  (gtk-progress-bar-set-discrete-blocks :void
					((pbar :pointer)
					 (blocks guint)))
  (gtk-progress-bar-set-activity-step :void
				      ((pbar :pointer)
				       (step guint)))
  (gtk-progress-bar-set-activity-blocks :void
					((pbar :pointer)
					 (blocks guint)))
  (gtk-progress-bar-update :void
			   ((pbar :pointer)
			    (percentage gdouble)))
  
  ;;image
  (gtk-image-new-from-file :pointer
			   ((filename gtk-string)))
  (gtk-image-new-from-stock :pointer
			    ((stock gtk-string)
			     (icon-size gtk-icon-size)))
  (gtk-image-set-from-stock :void
			    ((image :pointer)
			     (stock gtk-string)
			     (icon-size gtk-icon-size)))
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
  (gtk-statusbar-get-context-id guint
				((sbar :pointer)
				 (description gtk-string)))
  (gtk-statusbar-push guint
		      ((sbar :pointer)
		       (context-id guint)
		       (text gtk-string)))
  (gtk-statusbar-pop :void
		     ((sbar :pointer)
		      (context-id guint)))
  (gtk-statusbar-remove :void
			((sbar :pointer)
			 (context-id guint)
			 (message-id guint)))
  (gtk-statusbar-set-has-resize-grip :void
				     ((sbar :pointer)
				      (setting gboolean)))

  ;;widget
  (gtk-widget-show :void ((widget :pointer)))
  (gtk-widget-show-all :void ((widget :pointer)))
  (gtk-widget-hide :void ((widget :pointer)))
  (gtk-widget-hide-all :void ((widget :pointer)))
  (gtk-widget-hide-on-delete gboolean
			     ((widget :pointer)))
  (gtk-widget-destroy :void ((widget :pointer)))
  (gtk-widget-set-sensitive :void
			    ((widget :pointer)
			     (sensitive gboolean)))
  (gtk-widget-set-size-request :void
			       ((widget :pointer)
				(width gint)
				(height gint)))
  (gtk-widget-get-parent-window :pointer
				((widget :pointer)))
  (gtk-widget-add-accelerator :void
			      ((widget :pointer)
			       (gsignal gtk-string)
			       (accel-group :pointer)
			       (key guint)
			       (mods gdk-modifier-type)
			       (flags gtk-accel-flags)))
  (gtk-widget-grab-focus :void
			 ((widget :pointer)))
  (gtk-widget-add-events :void
			 ((widget :pointer)
			  (events gint)))
  (gtk-widget-set-events :void
			 ((widget :pointer)
			  (events gint)))
  (gtk-widget-get-events gint ((widget :pointer)))
  (gtk-widget-queue-draw :void ((widget :pointer)))

  ;;window
  (gtk-window-new :pointer ((type gtk-window-type)))
  (gtk-window-set-title :void
			((widget :pointer)
			 (title gtk-string)))
  (gtk-window-set-icon-from-file gboolean
				 ((window :pointer)
				  (filename gtk-string)
				  (err :pointer)))
  (gtk-window-set-default-size :void
			       ((widget :pointer)
				(width gint)
				(height gint)))
  (gtk-window-set-resizable :void
			    ((widget :pointer)
			     (resizable gboolean)))
  (gtk-window-set-decorated :void
			    ((widget :pointer)
			     (decorated gboolean)))
  (gtk-window-set-auto-startup-notification :void
					    ((setting gboolean)))
  (gtk-window-set-position :void
			   ((widget :pointer)
			    (position gtk-window-position)))
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
  (gtk-entry-get-text gtk-string ((entry :pointer)))
  (gtk-entry-set-max-length :void
			    ((entry :pointer)
			     (max-length gint)))
  (gtk-entry-set-editable :void
			  ((entry :pointer)
			   (editable
			    gboolean)))
  (gtk-entry-set-completion :void
			    ((entry :pointer)
			     (completion :pointer)))
  (gtk-entry-set-has-frame :void
			   ((entry :pointer)
			    (has-frame gboolean)))

  ;;entry-completion
  (gtk-entry-completion-new :pointer ())
  (gtk-entry-completion-set-model :void
				  ((completion :pointer)
				   (model :pointer)))
  (gtk-entry-completion-set-text-column :void
					((completion :pointer)
					 (column gint)))

  ;;range
  (gtk-range-set-range :void
		       ((range :pointer)
			(minval gdouble)
			(maxval gdouble)))
  (gtk-range-set-value :void
		       ((range :pointer)
			(val gdouble)))
  (gtk-range-set-inverted :void
			  ((range :pointer)
			   (inverted gboolean)))
  (gtk-range-set-increments :void
			    ((range :pointer)
			     (step gdouble)
			     (page gdouble)))
  (gtk-range-set-update-policy :void
			       ((range :pointer)
				(policy gtk-update-type)))
  (gtk-range-get-value gdouble ((range :pointer)))

  ;;scale
  (gtk-scale-set-draw-value :void
			    ((scale :pointer)
			     (draw-value gboolean)))
  (gtk-scale-set-value-pos :void
			   ((scale :pointer)
			    (pos-type gtk-position-type)))
  (gtk-scale-set-digits :void
			((scale :pointer)
			 (digits gint)))

  ;;hscale
  (gtk-hscale-new :pointer
		  ((adjustment :pointer)))
  (gtk-hscale-new-with-range :pointer
			     ((minval gdouble)
			      (maxval gdouble)
			      (step gdouble)))

  ;;vscale
  (gtk-vscale-new :pointer
		  ((adjustment :pointer)))
  (gtk-vscale-new-with-range :pointer
			     ((minval gdouble)
			      (maxval gdouble)
			      (step gdouble)))

  ;;combo-box
  (gtk-combo-box-new-text :pointer ())
  (gtk-combo-box-append-text :void
			     ((combo-box :pointer)
			      (text gtk-string)))
  (gtk-combo-box-remove-text :void
			     ((combo-box :pointer)
			      (position gint)))
  (gtk-combo-box-set-active :void
			    ((combo-box :pointer)
			     (index gint)))
  (gtk-combo-box-get-active gint
			    ((combo-box :pointer)))
  (gtk-combo-box-set-model :void
			   ((combo-box :pointer)
			    (model :pointer)))
  (gtk-combo-box-get-active-iter gboolean
				 ((combo-box :pointer)
				  (iter :pointer)))
  (gtk-combo-box-set-active-iter :void
				 ((combo-box :pointer)
				  (iter :pointer)))

  ;;combo-box-entry
  (gtk-combo-box-entry-new :pointer ())
  (gtk-combo-box-entry-new-text :pointer ())
  (gtk-combo-box-entry-new-with-model :pointer ((tree-model :pointer)
                                                (text-column gint)))
  (gtk-combo-box-entry-get-text-column gint
                                       ((combo-box-entry :pointer))) 
  (gtk-combo-box-entry-set-text-column :void
                                       ((combo-box-entry :pointer)
                                        (column gint)))

  ;;calendar
  (gtk-calendar-new :pointer ())
  (gtk-calendar-get-date :void
			 ((cal :pointer)
			  (year :pointer)
			  (month :pointer)
			  (day :pointer)))
  (gtk-calendar-select-month gboolean
			     ((cal :pointer)
			      (month guint)
			      (year guint)))
  (gtk-calendar-select-day :void
			   ((cal :pointer)
			    (day guint)))

  ;;arrow
  (gtk-arrow-new :pointer
		 ((arrow-type gtk-arrow-type)
		  (shadow-type gtk-shadow-type)))
  (gtk-arrow-set :void
		 ((arrow :pointer)
		  (arrow-type gtk-arrow-type)
		  (shadow-type gtk-shadow-type)))

  ;;dialog
  (gtk-dialog-new :pointer ())
  (gtk-dialog-run gint ((dialog :pointer)))
  (gtk-dialog-response :void
		       ((dialog :pointer)
			(response-id gint)))
  (gtk-dialog-add-button :pointer
			 ((dialog :pointer)
			  (button-text
			   gtk-string)
			  (response-id gint)))
  (gtk-dialog-add-action-widget :void
				((dialog :pointer)
				 (child :pointer)
				 (response-id gint)))
  (gtk-dialog-set-has-separator :void
				((dialog :pointer)
				 (has-separator gboolean)))
  (gtk-dialog-set-default-response :void
				   ((dialog :pointer)
				    (response-id gint)))

  ;;message-dialog
  (gtk-message-dialog-new :pointer
			  ((parent :pointer)
			   (flags gtk-dialog-flags)
			   (type gtk-message-type)
			   (buttons gtk-buttons-type)
			   (message gtk-string)))
  (gtk-message-dialog-set-markup :void
				 ((dialog :pointer)
				  (str gtk-string)))

  ;;file-chooser
  (gtk-file-chooser-set-action :void
			       ((chooser :pointer)
				(action gtk-file-chooser-action)))
  (gtk-file-chooser-set-local-only :void
				   ((chooser :pointer)
				    (local-only gboolean)))
  (gtk-file-chooser-set-select-multiple :void
					((chooser :pointer)
					 (select-multiple gboolean)))
  (gtk-file-chooser-set-current-name :void 
				     ((chooser :pointer)
				      (name gtk-string)))
  (gtk-file-chooser-set-filename gboolean 
				 ((chooser :pointer)
				  (filename gtk-string)))
  (gtk-file-chooser-get-filename gtk-string
				 ((chooser :pointer)))
  (gtk-file-chooser-get-filenames :pointer
				  ((chooser :pointer)))
  (gtk-file-chooser-set-current-folder gboolean
				       ((chooser :pointer)
					(folder gtk-string)))
  (gtk-file-chooser-get-current-folder gtk-string
				       ((chooser :pointer)))
  (gtk-file-chooser-set-uri gboolean
			    ((chooser :pointer)
			     (uri gtk-string)))
  (gtk-file-chooser-get-uri gtk-string
			    ((chooser :pointer)))
  (gtk-file-chooser-select-uri gboolean
			       ((chooser :pointer)
                                (uri :string)))
  (gtk-file-chooser-get-uris :pointer
			     ((chooser :pointer)))
  (gtk-file-chooser-set-current-folder-uri gboolean
					   ((chooser :pointer)
					    (folder gtk-string)))
  (gtk-file-chooser-get-current-folder-uri gtk-string
					   ((chooser :pointer)))
  (gtk-file-chooser-set-use-preview-label :void
					  ((chooser :pointer)
					   (use-label gboolean)))
  (gtk-file-chooser-add-filter :void
			       ((chooser :pointer)
				(filter :pointer)))
  (gtk-file-chooser-set-filter :void
			       ((chooser :pointer)
				(filter :pointer)))

  ;;file-chooser-widget
  (gtk-file-chooser-widget-new :pointer
			       ((action gtk-file-chooser-action)))

  ;;file-chooser-dialog
  (gtk-file-chooser-dialog-new :pointer
			       ((title gtk-string)
				(parent :pointer)
				(action gtk-file-chooser-action)
				(cancel-text gtk-string)
				(cancel-response-id gint)
				(accept-text gtk-string)
				(accept-response-id gint)
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
				(wrap-mode gtk-wrap-mode)))
  (gtk-text-view-set-editable :void
			      ((text-view :pointer)
			       (setting gboolean)))
  (gtk-text-view-scroll-to-iter gboolean
				((text-view :pointer)
				 (iter :pointer)
				 (within-margin gdouble)
				 (use-align gboolean)
				 (xalign gdouble)
				 (yalign gdouble)))
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
			     (include-hidden-chars gboolean)))
  (gtk-text-buffer-insert :void
			  ((buffer :pointer)
			   (iter :pointer)
			   (text gtk-string)
			   (len gint)))
  (gtk-text-buffer-delete :void
			  ((buffer :pointer)
			   (start :pointer)
			   (end :pointer)))
  (gtk-text-buffer-set-text :void
			    ((buffer :pointer)
			     (text gtk-string)
			     (len gint)))
  (gtk-text-buffer-get-char-count gint
				  ((buffer :pointer)))
  (gtk-text-buffer-insert-at-cursor :void
				    ((buffer :pointer)
				     (text :pointer)
				     (len gint)))
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
				       (char-offset gint)))
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
				(left-gravity gboolean)))
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
  (gtk-text-buffer-get-modified gboolean
				((buffer :pointer)))
  (gtk-text-buffer-set-modified :void
				((buffer :pointer)
				 (setting gboolean)))
  (gtk-text-buffer-move-mark :void
			     ((buffer :pointer)
			      (mark :pointer)
			      (where :pointer)))
  ;;text iter
  (gtk-text-iter-free :void ((iter :pointer)))
  (gtk-text-iter-get-offset gint
			    ((iter :pointer)))

  ;;text-tag-table
  (gtk-text-tag-table-new :pointer ())
  (gtk-accel-group-new :pointer ())

  ;;ui-manager
  (gtk-ui-manager-new :pointer ())
  (gtk-ui-manager-set-add-tearoffs :void
				   ((ui-manager :pointer)
				    (add-tearoffs gboolean)))
  (gtk-ui-manager-insert-action-group :void
				      ((ui-manager :pointer)
				       (action-group :pointer)
				       (pos gint)))
  (gtk-ui-manager-get-toplevels :pointer
				((ui-manager :pointer)
				 (types gtk-ui-manager-item-type)))

  ;;action-group
  (gtk-action-group-new :pointer
			((name gtk-string)))
  (gtk-action-group-set-sensitive :void
				  ((action-group :pointer)
				   (sensitive gboolean)))
  (gtk-action-group-set-visible :void
				((action-group :pointer)
				 (visible gboolean)))
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
				  (above gboolean)))
  (gtk-event-box-set-visible-window :void
				    ((event-box :pointer)
				     (visible-window gboolean)))

  ;;miscellaneous
  (gtk-check-version gtk-string
		     ((required-major guint)
		      (required-minor guint)
		      (required-micro guint)))
  (gtk-widget-create-pango-layout :pointer
				  ((widget :pointer)
				   (text gtk-string))))
;;; replacements for libcellsgtk
(def-gtk-lib-functions :cgtk
    (gtk-dialog-get-content-area :pointer ((dialog :pointer)))
    (gtk-text-iter-copy :pointer ((iter :pointer)))
    (gtk-tree-iter-copy :pointer ((iter :pointer)))
    (gtk-widget-get-mapped gboolean ((widget :pointer)))
    (gtk-widget-get-visible gboolean ((widget :pointer)))
    (gtk-widget-get-window :pointer ((widget :pointer)))
    (gtk-widget-get-allocation :pointer ((widget :pointer) (allocation :pointer))))

;; not sure if this is right, but the auxiliary C code does something equivalent

;; This allocates an iterator of proper size copying random memory spot. Not sure why the iterator
;; cannot be allocated directly, but GTK does not expose a 'new' function, and C code is supposed to
;; allocate iterators on stack. The 'copy' function is provided for language bindings, but I do not
;; see where to get size information, so I allocate a dummy block.
(defun gtk-adds-text-iter-new ()
  (cffi:with-foreign-pointer (dummy 4096)
    (loop for i from 0 below 512 do (setf (cffi:mem-aref dummy :uint64 i) 0))
    (gtk-text-iter-copy dummy)))

(defun gtk-adds-tree-iter-new ()
  (cffi:with-foreign-pointer (dummy 4096)
    (loop for i from 0 below 512 do (setf (cffi:mem-aref dummy :uint64 i) 0))
    (gtk-tree-iter-copy dummy)))

;; since this is in documentation I assume the layout is stable
(cffi:defcstruct gtk-allocation
  (x gint)
  (y gint)
  (width gint)
  (height gint))

(defun gtk-adds-widget-width (widget)
  (cffi:with-foreign-object (allocation 'gtk-allocation)
    (gtk-widget-get-allocation widget allocation)
    (cffi:foreign-slot-value allocation 'gtk-allocation 'width)))

(defun gtk-adds-widget-height (widget)
  (cffi:with-foreign-object (allocation 'gtk-allocation)
    (gtk-widget-get-allocation widget allocation)
    (cffi:foreign-slot-value allocation 'gtk-allocation 'height)))

(defun gtk-widget-size (widget)
  (cffi:with-foreign-object (allocation 'gtk-allocation)
    (gtk-widget-get-allocation widget allocation)
    (values (cffi:foreign-slot-value allocation 'gtk-allocation 'width)
            (cffi:foreign-slot-value allocation 'gtk-allocation 'height))))
