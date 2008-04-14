#|

 Cells Gtk

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

(defpackage :cells-gtk
  (:nicknames :cgtk)
  (:use :common-lisp :pod :cells :gtk-ffi :utils-kt)
  (:export   #:find-widget
	     #:with-widget
	     #:widget-value
	     #:with-widget-value
	     #:gtk-user-signals-quit
	     #:gtk-continuable-error
	     #:gtk-report-error
	     #:cgtk-set-active-item-by-path
	     #:gtk-combo-box-set-active
	     #:show-message
	     #:file-chooser
	     #:with-markup
	     #:push-message
	     #:pop-message
	     #:pulse
	     #:gtk-app
	     #:gtk-reset
	     #:def-cells-tree-view
	     #:def-cells-tree-node
	     #:cells-tree-view
	     #:cells-tree-node
	     #:cells-tree-store
	     #:cells-gtk-init

	     #:title
	     #:icon
	     #:tooltips
	     #:tooltips-enable
	     #:tooltips-delay
	     #:allocated-width
	     #:allocated-height
	     
	     #:start-app
	     #:start-win
	     #:stop-gtk-main
	     #:open-windows
	     #:init-gtk
	     #:close-all-windows
	     #:with-gdk-threads
	     #:gtk-global-callback-register
	     #:gtk-global-callback-funcall
	     #:def-populate-adds
	     #:populate-adds
	     #:with-text-iters
	     #:text-buffer-get-text
	     #:text-buffer-delete-text
	     #:text-buffer-insert-text
	     #:text-buffer-append-text
	     #:text-buffer-modified-p
	     #:text-view-scroll-to-position
	     #:gtk-text-buffer-get-iter-at-offset
	     #:gtk-text-buffer-get-start-iter
	     #:gtk-text-buffer-get-end-iter
	     #:gtk-text-buffer-create-mark
	     #:gtk-text-view-set-wrap-mode
	     #:gtk-text-view-set-editable
	     #:gtk-text-buffer-move-mark
	     #:gtk-text-view-scroll-mark-onscreen
	     #:mk-listbox
	     #:mk-treebox
	     #:def-columns
	     #:callback
	     #:callback-if
	     #:timeout-add
	     #:focus
	     #:widget-id
	   
	     ;; for drawing-area ph

	     #:cairo-drawing-area
	     #:export-to-file
					; primitives
	     #:line
	     #:rectangle
	     #:arc
	     #:path
	     #:arrow-line
	     #:text-label
					; object handling
	     #:move
	     #:mk-primitive
	     #:modify
	     #:remove-primitive
	     #:erase
					; create own primitive
	     #:def-mk-primitive
	     #:defmodify
	     #:defdraw
	     #:defprimitive
					; accessor funcs for primitives
	     #:rgb
	     #:alpha
	     #:fill-rgb
	     #:fill-alpha
	     #:fin-1
	     #:fin-2
	     #:filled
	     #:stroked
	     #:line-width
	     #:p1
	     #:p2
	     #:p
	     #:size
	     #:p-min
	     #:p-max
	     #:c-o-g
	     #:radius
	     #:angle
	     #:widget
	     #:anchor-point
	     #:docking-point
	     #:mouse-over-p ; does the mouse hover over the primitive?
	     #:dragged-p    ; is the primitive being dragged?
	     #:draggable
	     #:drag-start
	     #:selectable
	     #:selection
	     #:selected-p
					; for the widget
	     #:mouse-pos		; mouse pos over the widget
	     #:on-clicked ; (on-clicked [widget] [button] [primitive])
	     #:on-moved	 ; (on-moved [widget] [mouse-pos] [primitive])
	     #:on-dragged ; (on-dragged [widget] [button] [primitive] [start-pos] [end-pos])
	     #:hover	  ; the primitive the mouse is currently over
	     #:dragging	  ; the primitive currently being dragged

	     #:gl-drawing-area
	     #:with-gl-context
	     #:init
	     #:draw
	     ))
