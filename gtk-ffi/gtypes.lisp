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
(cffi:defctype gsize :ulong)
(cffi:defctype gssize :long)
(cffi:defctype goffset gint64)
(cffi:defctype gintptr :long)
(cffi:defctype guintptr :ulong)

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

;;; gobject

(cffi:defctype gtype gsize)

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
