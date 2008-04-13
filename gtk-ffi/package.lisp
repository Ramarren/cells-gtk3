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
  (:use :common-lisp :pod)
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
	   #:event-type))

