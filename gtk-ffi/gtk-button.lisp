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
  (gtk-button-new :pointer ())
  (gtk-button-set-label
   :void
   ((button :pointer) (label gtk-string)))
  (gtk-button-set-relief
   :void
   ((button :pointer) (style gtk-relief-style)))
  (gtk-button-set-use-stock
   :void
   ((button :pointer) (use-stock gboolean)))
  (gtk-button-set-use-underline
   :void
   ((button :pointer) (use-underline gboolean)))
  (gtk-toggle-button-new :pointer ())
  (gtk-toggle-button-set-mode
   :void
   ((button :pointer)
    (draw-indicator gboolean)))
  (gtk-toggle-button-set-active
   :void
   ((button :pointer) (active gboolean)))
  (gtk-toggle-button-get-active
   gboolean
   ((button :pointer)))
  (gtk-check-button-new :pointer ())
  (gtk-radio-button-new
   :pointer
   ((gslist :pointer)))
  (gtk-radio-button-new-from-widget
   :pointer
   ((radio-group :pointer)))
  (gtk-spin-button-new
   :pointer
   ((adjustment :pointer)
    (climb-rate gdouble)
    (digits guint)))
  (gtk-spin-button-new-with-range
   :pointer
   ((minval gdouble)
    (maxval gdouble)
    (step gdouble)))
  (gtk-spin-button-set-value :void
                             ((spin-button :pointer) (value gdouble)))
  (gtk-spin-button-get-value
   gdouble
   ((spin-button :pointer)))
  (gtk-spin-button-get-value-as-int
   gint
   ((spin-button :pointer)))
  (gtk-spin-button-set-wrap
   :void
   ((spin-button :pointer) (wrap gboolean))))


#+debugthis
(gtk-toggle-button-get-active ((button c-pointer))
     boolean)

#+not
(DEF-GTK-FUNCTION :GTK GTK-TOGGLE-BUTTON-GET-ACTIVE
                  :ARGUMENTS ((BUTTON C-POINTER))
                  :RETURN-TYPE BOOLEAN
                  :CALL-DIRECT T)
