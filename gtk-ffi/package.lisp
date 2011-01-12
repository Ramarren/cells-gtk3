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

(in-package :cl-user)

(defpackage :gtk-ffi
  (:use :common-lisp :pod :utils-kt)
  (:export #:+c-null+
	   #:int-slot-indexed
	   #:load-gtk-libs
	   #:uint
	   #:c-pointer
	   #:c-ptr-null
	   #:c-array-ptr
	   #:c-ptr
	   #:c-string
	   #:sint32
	   #:uint32
	   #:uint8
	   #:boolean
	   #:ulong
	   #:int
	   #:long
	   #:single-float
	   #:double-float
	   #:gtk-string
	   #:gboolean
	   #:otherwise
	   #:*gtk-debug*
	   #:load-gtk-libs
	   #:col-type-to-ffi-type
	   #:deref-pointer-runtime-typed
	   #:gtk-tree-iter
	   #:with-g-value
	   #:gtk-signal-connect
	   #:gtk-signal-connect-swap
	   #:gtk-object-set-property
	   #:with-gtk-string
	   #:get-gtk-string
	   #:to-gtk-string
	   #:with-gdk-threads
	   #:gdk-threads-register-gtk-main
	   #:make-gtk-tree-iter
	   #:with-tree-iter
	   #:gtk-widget-set-popup
	   #:gvi
	   #:gtk-list-store-new
	   #:gtk-list-store-set
	   #:gtk-list-store-set-items
	   #:gtk-tree-store-new
	   #:gtk-tree-store-set
	   #:gtk-tree-store-set-cell
	   #:gtk-tree-store-set-kids
	   #:gtk-tree-model-get-cell
	   #:gtk-tree-model-get-typed-item-value
	   #:gtk-cell-renderer-set-attribs
	   #:gtk-file-chooser-get-filenames-strs
	   #:gtk-drawing-set-handlers
	   #:gdk-event-button-x
	   #:gdk-event-button-y
	   #:gdk-event-button-type
	   #:gdk-event-button-button
	   #:gdk-event-button-time
	   #:gdk-event-button-state
	   #:gdk-event-motion-x
	   #:gdk-event-motion-y
	   #:event-type

	   #:gtk-gl-init
	   	   ;;exported elsewhere 
	   #:int-slot-indexed #:gtk-image-menu-item-new-with-label
	   #:gdk-event-button-type #:gtk-text-buffer-set-modified #:gtk-icon-factory-new
	   #:gtk-text-buffer-get-text #:gtk-icon-factory-add-default #:gtk-tree-view-get-selection
	   #:gtk-text-view-get-buffer #:gtk-tree-path-to-string #:gtk-notebook-set-show-border
	   #:gtk-file-chooser-set-current-folder-uri #:gtk-check-menu-item-set-active
	   #:gtk-progress-bar-set-bar-style #:gtk-widget-show-all
	   #:gtk-tree-view-column-set-reorderable #:gtk-event-box-set-above-child
	   #:gdk-event-motion-x #:gtk-tree-iter #:gtk-text-buffer-apply-tag
	   #:gtk-file-chooser-set-uri #:gtk-separator-tool-item-new #:gtk-file-chooser-dialog-new
	   #:gtk-file-chooser-get-uris #:gtk-window-set-decorated #:gtk-menu-new
	   #:gtk-scrolled-window-set-placement #:gtk-tree-view-expand-all
	   #:gtk-tree-view-column-pack-start #:gtk-radio-menu-item-get-group #:gtk-frame-new
	   #:c-string #:gtk-tool-button-set-label-widget #:gtk-separator-tool-item-set-draw
	   #:gdk-gl-drawable-is-double-buffered #:g-locale-from-utf8
	   #:gtk-file-chooser-set-use-preview-label #:gdk-threads-enter
	   #:gtk-tree-selection-get-mode #:gtk-dialog-set-has-separator #:gtk-tree-store-set
	   #:gtk-text-buffer-move-mark #:gtk-expander-new #:gtk-tree-view-column-set-cell-data-func
	   #:gtk-frame-set-shadow-type #:gtk-cell-renderer-set-fixed-size #:gtk-entry-set-editable
	   #:gdk-flush #:gtk-window-new #:gtk-text-view-set-wrap-mode #:gtk-menu-item-set-accel-path
	   #:gdk-threads-register-gtk-main #:gtk-list-store-set-value #:gtk-text-view-new
	   #:with-gdk-threads #:gtk-arrow-new #:gtk-menu-attach-to-widget #:gtk-widget-get-gl-window
	   #:gtk-icon-factory-add #:gtk-tree-view-get-path-at-pos #:gtk-statusbar-push
	   #:gtk-label-set-selectable #:gtk-combo-box-entry-new #:gtk-tree-view-new
	   #:gtk-tree-row-reference-new #:gtk-cell-renderer-set-attribs #:gtk-window-set-position
	   #:gtk-list-store-append #:gtk-text-buffer-new #:gtk-ui-manager-insert-action-group
	   #:gtk-calendar-select-month #:gtk-tree-view-column-set-title
	   #:gtk-progress-bar-set-discrete-blocks #:gtk-notebook-get-n-pages
	   #:gtk-toolbar-set-tooltips #:load-gtk-libs #:gtk-file-filter-set-name
	   #:gtk-cell-renderer-text-new #:gtk-notebook-get-nth-page #:gtk-image-new-from-file
	   #:gtk-text-buffer-create-tag #:gtk-statusbar-new #:gtk-range-set-update-policy
	   #:gtk-file-chooser-add-filter #:gtk-entry-set-completion #:gtk-tree-selection-select-path
	   #:gtk-radio-menu-item-new-with-label #:gtk-spin-button-get-value-as-int
	   #:gtk-widget-destroy #:gtk-main #:gtk-window-deiconify #:gtk-notebook-prepend-page
	   #:gtk-window-set-resizable #:gtk-range-set-range #:gtk-tree-store-set-cell
	   #:gtk-file-chooser-set-select-multiple #:gtk-menu-item-set-submenu
	   #:gtk-widget-get-window #:gdk-event-button-x #:gtk-tool-item-set-homogeneous
	   #:gtk-text-buffer-get-iter-at-mark #:gtk-tool-item-set-expand
	   #:gtk-menu-item-remove-submenu #:gtk-check-menu-item-new-with-label
	   #:gtk-notebook-insert-page #:gtk-tree-view-column-set-expand
	   #:gtk-action-group-set-visible #:gtk-action-group-add-action-with-accel
	   #:gdk-event-button-y #:col-type-to-ffi-type #:gtk-cell-renderer-toggle-new #:sint32
	   #:gtk-statusbar-pop #:gtk-adds-ok #:gtk-box-pack-start-defaults
	   #:gtk-tree-view-column-set-visible #:gtk-widget-get-visible #:make-gtk-tree-iter
	   #:gboolean #:gtk-ui-manager-set-add-tearoffs #:gtk-file-chooser-get-current-folder
	   #:gtk-combo-box-entry-set-text-column #:g-thread-init #:gtk-event-box-new
	   #:gtk-label-set-text-with-mnemonic #:gtk-entry-set-text #:gtk-hpaned-new #:gtk-paned-add2
	   #:gtk-frame-set-label-widget #:gtk-arrow-set #:gtk-menu-item-get-submenu #:g-timeout-add
	   #:gtk-calendar-get-date #:gtk-button-set-use-stock
	   #:gtk-text-view-scroll-mark-onscreen #:gtk-widget-grab-focus #:gtk-drawing-area-new
	   #:gtk-widget-queue-draw #:gtk-hscale-new-with-range
	   #:gtk-dialog-get-content-area #:gtk-file-chooser-get-uri #:gtk-dialog-new #:gtk-menu-item-new
	   #:gtk-spin-button-get-value #:gtk-range-get-value #:gtk-tooltips-enable
	   #:gtk-notebook-new #:gtk-icon-set-new-from-pixbuf #:gtk-widget-set-gl-capability
	   #:g-value-set-boolean #:gtk-window-unmaximize #:gtk-entry-completion-set-model
	   #:gtk-hbox-new #:gtk-main-iteration #:gtk-scrolled-window-add-with-viewport
	   #:gtk-toggle-button-set-active #:gtk-box-set-homogeneous #:gtk-tree-view-remove-column
	   #:gtk-tree-row-reference-get-model #:gtk-tree-path-new-from-string
	   #:gtk-combo-box-entry-new-with-model #:gtk-vscale-new
	   #:gtk-image-menu-item-new-from-stock #:gtk-tree-view-column-set-max-width
	   #:gtk-text-buffer-remove-all-tags #:gtk-button-set-label #:c-ptr-null
	   #:gtk-table-attach-defaults #:gtk-widget-set-size-request #:gtk-text-view-set-editable
	   #:gtk-notebook-insert-page-menu #:gtk-tree-selection-selected-foreach
	   #:gtk-text-buffer-insert #:gtk-tree-view-column-set-fixed-width
	   #:gtk-notebook-get-current-page #:gtk-container-get-resize-mode #:to-gtk-string
	   #:gdk-threads-init #:gtk-tooltips-set-delay #:gtk-toolbar-set-style
	   #:gtk-vscale-new-with-range #:gtk-menu-attach #:gtk-tree-selection-get-selected
	   #:gtk-quit-remove #:+c-null+ #:gtk-tearoff-menu-item-new #:boolean
	   #:gtk-text-buffer-get-modified #:gtk-adds-widget-height #:gtk-tool-button-new-from-stock
	   #:gtk-expander-set-use-underline #:c-array-ptr #:gtk-tooltips-new #:g-value-init
	   #:gtk-notebook-set-tab-pos #:get-gtk-string #:gtk-toggle-button-new #:gtk-tree-model-get
	   #:gtk-combo-box-set-active-iter #:gtk-dialog-set-default-response
	   #:gtk-misc-set-alignment #:gtk-tool-button-set-label #:gtk-init-check
	   #:gtk-tree-view-column-set-sort-indicator #:gtk-text-view-set-buffer
	   #:gtk-check-menu-item-new #:gtk-expander-set-label #:gtk-tree-model-get-typed-item-value
	   #:gtk-dialog-add-action-widget #:gdk-event-button-state #:gtk-window-set-title
	   #:gtk-tree-row-reference-free #:gtk-tree-store-clear #:g-value-set-string
	   #:gtk-image-menu-item-new #:gtk-tree-row-reference-get-path #:gtk-progress-bar-set-text
	   #:gdk-event-button-button #:otherwise #:gtk-signal-connect-swap
	   #:gtk-radio-button-new-from-widget #:gdk-gl-context-destroy
	   #:gdk-gl-drawable-swap-buffers #:gtk-list-store-set #:gtk-menu-shell-append
	   #:gtk-widget-get-gl-context #:gtk-tool-button-set-stock-id #:gtk-label-set-markup
	   #:gtk-check-button-new #:gtk-file-chooser-select-uri #:gtk-file-chooser-set-filter
	   #:gtk-progress-bar-new #:gtk-expander-set-use-markup #:gtk-tree-view-column-set-resizable
	   #:gtk-tree-view-column-new #:gtk-action-group-add-action #:gtk-text-buffer-get-mark
	   #:gtk-text-buffer-set-text #:gtk-tree-view-tree-to-widget-coords #:gtk-paned-set-position
	   #:gtk-toolbar-set-orientation #:gtk-tool-item-new #:gtk-tree-view-column-add-attribute
	   #:c-pointer #:g-value-set-float #:gtk-tree-model-get-iter-from-string #:uint32
	   #:gtk-tree-path-free #:gtk-text-buffer-get-end-iter #:gtk-scrolled-window-set-policy
	   #:gtk-drawing-set-handlers #:single-float #:gtk-scrolled-window-new
           #:int #:gtk-file-chooser-widget-new #:gtk-accel-label-new
	   #:gtk-spin-button-new #:gtk-separator-menu-item-new #:gdk-gl-config-is-rgba
	   #:gtk-box-pack-start #:gtk-accel-label-set-accel-widget #:gtk-widget-add-events
	   #:with-g-value #:with-tree-iter #:gtk-check-menu-item-get-active
	   #:gtk-progress-bar-set-activity-blocks #:gtk-file-chooser-get-current-folder-uri
	   #:gtk-label-set-use-markup #:gtk-entry-get-text #:gtk-container-set-border-width
	   #:gtk-ui-manager-get-toplevels #:gtk-combo-box-new-text
	   #:gtk-tree-view-widget-to-tree-coords #:gtk-toolbar-insert
	   #:gtk-file-chooser-set-current-folder #:gtk-vpaned-new #:gtk-combo-box-set-active
	   #:gtk-notebook-set-tab-border #:gtk-widget-show #:gtk-toolbar-new
	   #:gtk-file-chooser-get-filenames #:gtk-button-new #:gtk-events-pending
	   #:gtk-list-store-set-items #:gtk-box-set-spacing #:g-free
	   #:gtk-text-buffer-get-iter-at-offset #:gtk-label-set-text #:gtk-tree-model-get-iter
	   #:gtk-tool-button-set-use-underline #:gtk-ui-manager-new
	   #:gtk-radio-menu-item-new-from-widget #:gtk-dialog-run #:gtk-adds-widget-width
           #:gtk-widget-size
	   #:gtk-radio-menu-item-new-with-label-from-widget #:gtk-tree-store-new
	   #:gtk-image-get-pixbuf #:gtk-window-unfullscreen #:gtk-text-view-scroll-to-iter
	   #:gtk-notebook-set-show-tabs #:gtk-list-store-new #:gtk-notebook-append-page
	   #:gtk-adds-text-iter-new #:gtk-table-attach #:gdk-gl-window-new #:gtk-main-iteration-do
	   #:g-value-set-int #:gtk-image-menu-item-set-image #:gtk-toggle-button-set-mode
	   #:gtk-vseparator-new #:g-value-set-double #:gtk-cell-renderer-pixbuf-new #:gtk-main-quit
	   #:gtk-scale-set-draw-value #:gtk-combo-box-remove-text
	   #:gdk-gl-config-new-by-mode-for-screen #:gtk-radio-button-new #:gtk-dialog-add-button
	   #:gtk-file-filter-add-pattern #:gtk-menu-shell-prepend #:gtk-table-new
	   #:gtk-menu-item-set-right-justified #:gdk-event-button-time #:gtk-tree-store-set-value
	   #:gtk-combo-box-entry-new-text #:gtk-menu-set-title #:gtk-tree-view-column-set-min-width
	   #:gtk-widget-create-pango-layout #:gtk-tool-button-set-icon-widget #:g-value-set-long
	   #:gtk-radio-menu-item-new #:gtk-combo-box-get-active #:gtk-vbox-new #:gtk-check-version
	   #:gtk-tree-store-remove #:gtk-window-fullscreen #:gtk-entry-set-has-frame #:event-type
	   #:gtk-statusbar-get-context-id #:gtk-label-set-line-wrap #:gtk-main-level
	   #:gtk-gl-init-check #:gtk-dialog-response #:gtk-entry-new #:gtk-init #:gtk-gl-init
	   #:gtk-window-set-auto-startup-notification #:gtk-notebook-set-scrollable
	   #:gtk-action-group-new #:gtk-widget-hide-on-delete #:gtk-action-new #:g-cclosure-new-swap
	   #:gtk-notebook-popup-enable #:deref-pointer-runtime-typed #:gtk-tool-button-new
	   #:gtk-tree-view-column-set-spacing #:g-object-set-property #:gtk-alignment-new
	   #:gtk-get-current-event-time #:gtk-file-chooser-set-local-only
	   #:gtk-label-set-markup-with-mnemonic #:gtk-file-chooser-set-action
	   #:gtk-text-buffer-get-char-count #:gtk-expander-set-spacing #:gtk-text-iter-get-offset
	   #:gdk-gl-drawable-gl-begin #:gtk-notebook-append-page-menu #:gtk-calendar-new
	   #:gtk-button-set-relief #:double-float #:gtk-file-chooser-set-current-name
	   #:gtk-widget-set-popup #:gtk-label-new #:gtk-file-chooser-get-filenames-strs
	   #:gtk-tree-iter-free #:gtk-entry-completion-set-text-column
	   #:gtk-file-filter-add-mime-type #:gtk-accel-group-new #:gtk-action-group-set-sensitive
	   #:gtk-widget-get-events #:gtk-tree-model-get-cell #:gdk-gl-context-new
	   #:gtk-calendar-select-day #:gtk-paned-add1 #:gtk-tree-store-set-kids
	   #:gtk-entry-set-max-length #:gtk-widget-set-sensitive #:gtk-container-get-border-width
	   #:*gtk-debug* #:gtk-menu-shell-insert #:gdk-threads-leave #:c-ptr
	   #:gtk-message-dialog-set-markup #:gtk-window-set-icon-from-file #:gtk-text-iter-free
	   #:gtk-frame-set-label #:gtk-container-set-resize-mode #:gtk-image-set-from-stock
	   #:gtk-notebook-set-current-page #:gtk-tree-store-newv #:gtk-scale-set-digits
	   #:gtk-window-set-default-size #:gtk-list-store-newv #:gtk-progress-bar-pulse
	   #:with-gtk-string #:gdk-gl-drawable-gl-end #:gtk-list-store-clear #:gtk-widget-set-events
	   #:gdk-gl-config-is-double-buffered #:gtk-adds-color-set-rgb #:gtk-expander-set-expanded
	   #:gtk-entry-completion-new #:g-value-unset #:gtk-widget-get-mapped
	   #:gtk-tree-row-reference-valid #:gtk-image-new-from-stock #:g-locale-to-utf8
	   #:gtk-progress-bar-set-orientation #:gtk-range-set-value #:gtk-quit-add
	   #:gtk-tree-view-column-set-clickable #:gtk-file-filter-new #:gtk-tree-view-set-model
	   #:gtk-adds-tree-iter-new #:ulong #:gtk-accel-map-add-entry
	   #:gtk-statusbar-set-has-resize-grip #:gtk-range-set-inverted
	   #:gtk-spin-button-new-with-range #:gtk-aspect-frame-new #:gtk-window-maximize
	   #:gtk-tree-model-get-path #:gtk-tree-view-column-set-sort-column-id
	   #:gtk-combo-box-entry-get-text-column #:gtk-window-iconify #:long
	   #:gtk-expander-set-label-widget #:gtk-menu-item-new-with-label #:gtk-tree-view-get-model
	   #:gtk-tooltips-set-tip #:gtk-notebook-popup-disable
	   #:gtk-text-buffer-delete #:gtk-progress-bar-update #:g-cclosure-new
	   #:gtk-toggle-button-get-active #:gtk-tree-selection-set-mode
	   #:gtk-text-buffer-create-mark #:gtk-file-chooser-get-filename #:uint #:gtk-alignment-set
	   #:gtk-menu-popup #:gtk-event-box-set-visible-window #:gtk-progress-bar-set-fraction
	   #:gtk-statusbar-remove #:uint8 #:gtk-notebook-set-homogeneous-tabs #:gvi
	   #:gtk-text-tag-table-new #:gtk-combo-box-append-text #:gtk-signal-connect
	   #:gtk-window-add-accel-group #:gtk-combo-box-get-active-iter #:gtk-combo-box-set-model
	   #:gtk-progress-bar-set-pulse-step #:gtk-menu-bar-new #:g-slist-free
           #:gtk-progress-bar-set-percentage #:gtk-progress-bar-set-value
	   #:gdk-gl-config-new-by-mode #:gdk-event-motion-y #:gtk-tool-item-set-is-important
	   #:gtk-tree-store-append #:g-signal-connect-closure #:gtk-container-remove
	   #:gtk-misc-set-padding #:gtk-text-buffer-insert-at-cursor #:gtk-container-add
	   #:gtk-object-set-property #:gtk-widget-hide-all #:gtk-table-set-homogeneous #:gtk-string
	   #:gtk-file-chooser-set-filename #:gtk-adds-color-new #:gtk-scale-set-value-pos
	   #:gtk-message-dialog-new #:gtk-spin-button-set-wrap #:gtk-tool-item-set-tooltip
	   #:gtk-toolbar-set-show-arrow #:gtk-hscale-new #:gtk-progress-bar-set-activity-step
	   #:gtk-scrolled-window-set-shadow-type #:gtk-text-buffer-get-insert
	   #:gtk-text-buffer-delete-mark #:gtk-hseparator-new #:gtk-notebook-remove-page
	   #:gtk-widget-add-accelerator #:gtk-widget-get-parent-window #:gtk-tree-view-insert-column
	   #:gtk-action-group-remove-action #:gtk-spin-button-set-value #:gtk-frame-set-label-align
	   #:gtk-widget-hide #:gtk-tooltips-disable #:gtk-range-set-increments
	   #:gtk-notebook-prepend-page-menu #:gdk-gl-window-destroy
           #:gtk-text-iter-copy #:gtk-widget-get-allocation
           #:g-object-ref-sink
           #:gtk-bin-get-child
           #:gtk-button-set-use-underline
           #:g-object-unref
           #:g-object-is-floating #:g-object-ref
           #:gtk-tree-view-append-column
           #:g-thread-get-initialized
           #:gtk-tree-iter-copy))
