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

(in-package :gtk-ffi)

;;;; Library loading

;;; note: on Intel Macs sbcl built from Macports has both unix and darwin features, also I think
;;; there is Quartz based GTK

(cffi:define-foreign-library :gobject
  (:darwin "libgobject-2.0.dylib")
  (:unix (:or "libgobject-2.0.so" "libgobject-2.0.so.0"))
  (:windows "libgobject-2.0-0.dll"))

(cffi:define-foreign-library :glib
  (:darwin "libglib-2.0.dylib")
  (:unix (:or "libglib-2.0.so" "libglib-2.0.so.0"))
  (:windows "libglib-2.0-0.dll"))

(cffi:define-foreign-library :gthread
  (:darwin "libgthread-2.0.dylib")
  (:unix (:or "libgthread-2.0.so" "libgthread-2.0.so.0"))
  (:windows "libgthread-2.0-0.dll"))

(cffi:define-foreign-library :gdk
  (:darwin "libgdk-x11-2.0.dylib")
  (:unix (:or "libgdk-x11-2.0.so" "libgdk-x11-2.0.so.0"))
  (:windows "libgdk-win32-2.0-0.dll"))

(cffi:define-foreign-library :gtk
  (:darwin "libgtk-x11-2.0.dylib")
  (:unix (:or "libgtk-x11-2.0.so" "libgtk-x11-2.0.so.0"))
  (:windows "libgtk-win32-2.0-0.dll"))

;;; comment moved from gtk-ffi.lisp
;;; LW Win32 is hanging on POD's machine only:
;;; (fli:register-module "libgdk-win32-2.0-0.dll" :connection-style :immediate)
;;; (fli:register-module "c:\\Program Files\\Common Files\\GTK\\2.0\\bin\\libgdk-win32-2.0-0.dll" 
;;;                      :connection-style :immediate)

(defun load-gtk-libs ()
  (handler-bind ((style-warning #'muffle-warning))
    (cffi:load-foreign-library :gobject)
    (cffi:load-foreign-library :glib)
    (cffi:load-foreign-library :gthread)
    (cffi:load-foreign-library :gdk)
    (cffi:load-foreign-library :gtk)))