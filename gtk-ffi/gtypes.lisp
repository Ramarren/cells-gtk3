#|

 Gtk ffi

 Copyright (c) 2009 by Jakub Higersberger <ramarren@gmail.com>

 You have the right to distribute and use this software as governed by 
 the terms of the Lisp Lesser GNU Public License (LLGPL):

    (http://opensource.franz.com/preamble.html)
 
 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 Lisp Lesser GNU Public License for more details.
 
|#

;;;; Type GLIB type definitions

;;;; cffi-grovel would be helpful here, but would add dependency on C compiler

(in-package :gtk-ffi)

;;; constants

;;keep SBCL happy
(defconstant +c-null+ 
  (if (boundp '+c-null+)
      (symbol-value '+c-null+)
    (cffi:null-pointer)))

(defconstant +true+ 1)
(defconstant +false+ 0)

;;; basic types

(cffi:defctype gpointer :pointer)
(cffi:defctype gconstpointer :pointer)
(cffi:defctype gchar :char)
(cffi:defctype guchar :uchar)
(cffi:defctype gint :int)
(cffi:defctype guint :uint)
(cffi:defctype gshort :short)
(cffi:defctype gushort :ushort)
(cffi:defctype glong :long)
(cffi:defctype gulong :ulong)
(cffi:defctype gint8 :int8)
(cffi:defctype guint8 :uint8)
(cffi:defctype gint16 :int16)
(cffi:defctype guint16 :uint16)
(cffi:defctype gint32 :int32)
(cffi:defctype guint32 :uint32)
(cffi:defctype gint64 :int64)
(cffi:defctype guint64 :uint64)
(cffi:defctype gfloat :float)
(cffi:defctype gdouble :double)
(cffi:defctype gsize :uint)
(cffi:defctype gssize :int)
(cffi:defctype goffset gint64)
(cffi:defctype gintptr :int)
(cffi:defctype guintptr :uint)

;;; types with translation

(cffi:define-foreign-type gboolean ()
  ()
  (:actual-type gint)
  (:simple-parser gboolean))

(cffi:define-parse-method gboolean ()
  (make-instance 'gboolean))

(defmethod cffi:translate-to-foreign (value (type gboolean))
  (if value +true+ +false+))

(defmethod cffi:translate-from-foreign (value (type gboolean))
  (cond
    ((eql value +false+) nil)
    (t t)))

(cffi:define-foreign-type gtk-string ()
  ()
  (:actual-type :pointer)
  (:simple-parser gtk-string))

(defmethod cffi:translate-to-foreign (value (type gtk-string))
  (cffi:foreign-string-alloc (or value "") :encoding :utf-8))

(defmethod cffi:translate-from-foreign (value (type gtk-string))
  (cffi:foreign-string-to-lisp value :encoding :utf-8))

;; for return caller owned return values
(cffi:define-foreign-type gtk-string-owned ()
  ()
  (:actual-type :pointer)
  (:simple-parser gtk-string-owned))

(defmethod cffi:translate-from-foreign (value (type gtk-string-owned))
  (prog1
      (cffi:foreign-string-to-lisp value :encoding :utf-8)
    (cffi:foreign-string-free value)))

;;; gobject

#+cells-gtk-gtype-is-gulong (cffi:defctype gtype gulong)
#-cells-gtk-gtype-is-gulong (cffi:defctype gtype gsize)

;;; enums

(cffi:defcenum gdk-event-type
  (:nothing -1)
  (:delete 0)
  (:destroy 1)
  (:expose 2)
  (:notify 3)                           ; that is, pointer motion notify
  (:button_press 4)
  (:2button_press 5)
  (:3button_press 6)
  (:button_release 7)
  (:key_press 8)
  (:key_release 9)
  (:enter_notify 10)
  (:leave_notify 11)
  (:focus_change 12)
  (:configure 13)
  (:map 14)
  (:unmap 15)
  (:property_notify 16)
  (:selection_clear 17)
  (:selection_request 18)
  (:selection_notify 19)
  (:proximity_in 20)
  (:proximity_out 21)
  (:drag_enter 22)
  (:drag_leave 23)
  (:drag_motion 24)
  (:drag_status 25)
  (:drop_start 26)
  (:drop_finished 27)
  (:client_event 28)
  (:visibility_notify 29)
  (:no_expose 30)
  (:scroll 31)
  (:window_state 32)
  (:setting 33))

(cffi:defcenum gtk-resize-mode
  :parent
  :queue
  :immediate)

(cffi:defcenum gtk-attach-options
  (:expand 1)
  (:shrink 2)
  (:fill 4))

(cffi:defcenum gtk-shadow-type
  :none :in :out :etched-in :etched-out)

(cffi:defcenum gtk-policy-type
  :always :automatic :never)

(cffi:defcenum gtk-corner-type
  :top-left :bottom-left :top-right :bottom-right)

(cffi:defcenum gtk-position-type
  :left :right :top :bottom)

(cffi:defcenum gtk-orientation
  :horizontal :vertical)

(cffi:defcenum gtk-toolbar-style
  :icons :text :both :both-horiz)

(cffi:defcenum gtk-relief-style
  :normal :half :none)

(cffi:defcenum gtk-progress-bar-orientation
  :left-to-right :right-to-left :bottom-to-top :top-to-bottom)

(cffi:defcenum gtk-progress-bar-style
  :continuous :discrete)

(cffi:defcenum gtk-icon-size
  :invalid :menu :small-toolbar :large-toolbar :button :dnd :dialog)

(cffi:defcenum gdk-modifier-type
  (:shift-mask     #.(ash 1 0))
  (:lock-mask	  #.(ash 1 1))
  (:control-mask   #.(ash 1 2))
  (:mod1-mask	  #.(ash 1 3))
  (:mod2-mask	  #.(ash 1 4))
  (:mod3-mask	  #.(ash 1 5))
  (:mod4-mask	  #.(ash 1 6))
  (:mod5-mask	  #.(ash 1 7))
  (:button1-mask   #.(ash 1 8))
  (:button2-mask   #.(ash 1 9))
  (:button3-mask   #.(ash 1 10))
  (:button4-mask   #.(ash 1 11))
  (:button5-mask   #.(ash 1 12))
  (:super-mask     #.(ash 1 26))
  (:hyper-mask     #.(ash 1 27))
  (:meta-mask      #.(ash 1 28))
  (:release-mask   #.(ash 1 30))
  (:modifier-mask  #x5c001fff))

(cffi:defcenum gtk-accel-flags
  (:visible 1)
  (:locked 2)
  (:mask #x07))

(cffi:defcenum gtk-window-position
  :none :center :mouse :center-always :center-on-parent)

(cffi:defcenum gtk-window-type
  :toplevel :popup)

(cffi:defcenum gtk-update-type
  :continuous :discontinuous :delayed)

(cffi:defcenum gtk-arrow-type
  :up :down :left :right :none)

(cffi:defcenum gtk-dialog-flags
  (:modal 1)
  (:destroy-with-parent 2)
  (:no-separator 4))

(cffi:defcenum gtk-message-type
  :info :warning :question :error :other)

(cffi:defcenum gtk-buttons-type
  :none :ok :close :cancel :yes-no :ok-cancel)

(cffi:defcenum gtk-file-chooser-action
  :open :save :select-folder :create-folder)

(cffi:defcenum gtk-wrap-mode
  :none :char :word :word-char)

(cffi:defcenum gtk-ui-manager-item-type
  (:auto 0)
  (:menubar #.(ash 1 0))
  (:menu #.(ash 1 1))
  (:toolbar #.(ash 1 2))
  (:placeholder #.(ash 1 3))
  (:popup #.(ash 1 4))
  (:menuitem #.(ash 1 5))
  (:toolitem #.(ash 1 6))
  (:separator #.(ash 1 7))
  (:accelerator #.(ash 1 8))
  (:popup-with-accels #.(ash 1 9)))

(cffi:defcenum gtk-selection-mode
  :none :single :browse :multiple :extended)

;;; unions

(cffi:defcunion g-value-data
  (v-int gint)
  (v-uint guint)
  (v-long glong)
  (v-ulong gulong)
  (v-int64 gint64)
  (v-uint64 guint64)
  (v-float gfloat)
  (v-double gdouble)
  (v-pointer gpointer))
