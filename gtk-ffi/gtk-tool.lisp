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
  ;;toolbar
  (gtk-toolbar-new :pointer ())
  (gtk-toolbar-insert
   :void
   ((toolbar :pointer)
    (item :pointer)
    (pos :int)))
  (gtk-toolbar-set-show-arrow
   :void
   ((toolbar :pointer)
    (show-arrow :gtk-boolean)))
  (gtk-toolbar-set-orientation
   :void
   ((toolbar :pointer) (orientation :int)))
  (gtk-toolbar-set-tooltips
   :void
   ((toolbar :pointer) (enable :gtk-boolean)))
  (gtk-toolbar-set-style
   :void
   ((toolbar :pointer) (style :int)))

  ;;tooltips
  (gtk-tooltips-new :pointer ())
  (gtk-tooltips-set-tip
   :void
   ((tooltips :pointer)
    (widget :pointer)
    (tip-text :gtk-string)
    (tip-private :gtk-string)))
  (gtk-tooltips-enable
   :void
   ((tooltips :pointer)))
  (gtk-tooltips-disable
   :void
   ((tooltips :pointer)))
  (gtk-tooltips-set-delay
   :void
   ((tooltips :pointer) (delay :unsigned-int)))

  ;;tool-item
  (gtk-tool-item-new :pointer ())
  (gtk-tool-item-set-homogeneous
   :void
   ((tool-item :pointer)
    (homogeneous :gtk-boolean)))
  (gtk-tool-item-set-expand
   :void
   ((tool-item :pointer) (expand :gtk-boolean)))
  (gtk-tool-item-set-tooltip
   :void
   ((tool-item :pointer)
    (tooltips :pointer)
    (tip-text :gtk-string)
    (tip-private :gtk-string)))
  (gtk-tool-item-set-is-important
   :void
   ((tool-item :pointer)
    (is-important :gtk-boolean)))
  (gtk-separator-tool-item-new :pointer ())
  (gtk-separator-tool-item-set-draw
   :void
   ((item :pointer) (draw :gtk-boolean)))

  ;;tool-button
  (gtk-tool-button-new
   :pointer
   ((icon-widget :pointer) (label :pointer)))
  (gtk-tool-button-new-from-stock
   :pointer
   ((stock-id :gtk-string)))
  (gtk-tool-button-set-label
   :void
   ((tool-button :pointer) (label :pointer)))
  (gtk-tool-button-set-use-underline
   :void
   ((tool-button :pointer)
    (use-underline :gtk-boolean)))
  (gtk-tool-button-set-stock-id
   :void
   ((tool-button :pointer) (stock-id :gtk-string)))
  (gtk-tool-button-set-icon-widget
   :void
   ((tool-button :pointer)
    (icon-widget :pointer)))
  (gtk-tool-button-set-label-widget
   :void
   ((tool-button :pointer)
    (label-widget :pointer))))