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

(def-gtk-function :gtk gtk-check-menu-item-set-active :void
  ((check-menu :pointer) (active gboolean)))

#+test
(def-gtk-lib-functions :gtk
  (gtk-check-menu-item-set-active ((check-menu c-pointer)
                                   (active boolean))))

(def-gtk-lib-functions :gtk
  (gtk-menu-shell-append :void
                           ((menu-shell :pointer)
                            (child :pointer)))
  (gtk-menu-shell-prepend :void
                          ((menu-shell :pointer)
                           (child :pointer)))
  (gtk-menu-shell-insert :void
                         ((menu-shell :pointer)
                          (child :pointer)
                          (position gint)))
  (gtk-menu-bar-new :pointer ())
  (gtk-menu-new :pointer ())
  (gtk-menu-set-title :void
                      ((menu :pointer) (title gtk-string)))
  (gtk-menu-attach :void
                   ((menu :pointer) (child :pointer)
                    (lattach guint)
                    (rattach guint)
                    (tattach guint)
                    (battach guint)))
  (gtk-menu-attach-to-widget :void
                             ((menu :pointer)
                              (widget :pointer)
                              (func :pointer)))
  ;;func = (callback-function ((menu c-pointer) (x (c-ptr int)) (y (c-ptr int))
  ;;                           (push-in (c-ptr boolean)) (data c-pointer)))
  (gtk-menu-popup :void
                  ((menu :pointer) (p-menu-shell :pointer)
                   (p-menu-item :pointer) (func :pointer)
                   (data gpointer) (button guint)
                   (activate-time guint32)))
  (gtk-menu-item-new :pointer ())
  (gtk-menu-item-new-with-label :pointer
                                ((label :pointer)))
  (gtk-menu-item-set-right-justified :void
                                     ((menu-item :pointer)
                                      (right-justified gboolean)))
  (gtk-menu-item-set-submenu :void
                             ((menu-item :pointer)
                              (submenu :pointer)))
  (gtk-menu-item-get-submenu :pointer
                             ((menu-item :pointer)))
  (gtk-menu-item-remove-submenu :void
                                ((menu-item :pointer)))
  (gtk-menu-item-set-accel-path :void
                                ((menu-item :pointer)
                                 (acell-path :pointer)))
  (gtk-accel-map-add-entry :void
                           ((accel-path gtk-string)
                            (accel-key guint)
                            (accel-mods gdk-modifier-type)))
  (gtk-check-menu-item-new :pointer ())
  (gtk-check-menu-item-new-with-label :pointer
                                      ((label :pointer)))
  (gtk-check-menu-item-get-active gboolean
                                  ((check-menu :pointer)))
  (gtk-radio-menu-item-new :pointer ((group :pointer)))
  (gtk-radio-menu-item-new-from-widget :pointer
                                       ((group :pointer)))
  (gtk-radio-menu-item-new-with-label :pointer
                                      ((group :pointer)
                                       (label gtk-string)))
  (gtk-radio-menu-item-new-with-label-from-widget :pointer
                                                  ((radio :pointer)
                                                   (label gtk-string)))
  (gtk-radio-menu-item-get-group :pointer ((radio :pointer)))
  (gtk-image-menu-item-new :pointer ())
  (gtk-image-menu-item-new-with-label :pointer ((label gtk-string)))
  (gtk-image-menu-item-new-from-stock :pointer
                                      ((stock-id gtk-string)
                                       (accel-group :pointer)))
  (gtk-image-menu-item-set-image :void
                                 ((menu-item :pointer)
                                  (image :pointer)))
  (gtk-separator-menu-item-new :pointer ())
  (gtk-tearoff-menu-item-new :pointer ()))
