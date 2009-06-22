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