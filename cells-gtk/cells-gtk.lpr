;; -*- lisp-version: "7.0 [Windows] (Dec 28, 2004 17:34)"; cg: "1.54.2.17"; -*-

(in-package :cg-user)

(defpackage :CELLS-GTK)

(define-project :name :cells-gtk
  :modules (list (make-instance 'module :name "cells-gtk.lisp")
                 (make-instance 'module :name "widgets.lisp")
                 (make-instance 'module :name "layout.lisp")
                 (make-instance 'module :name "display.lisp")
                 (make-instance 'module :name "buttons.lisp")
                 (make-instance 'module :name "entry.lisp")
                 (make-instance 'module :name "tree-view.lisp")
                 (make-instance 'module :name "menus.lisp")
                 (make-instance 'module :name "dialogs.lisp")
                 (make-instance 'module :name "textview.lisp")
                 (make-instance 'module :name "addon.lisp")
                 (make-instance 'module :name "gtk-app.lisp"))
  :projects (list (make-instance 'project-module :name
                                 "c:\\cell-cultures\\cells\\cells")
                  (make-instance 'project-module :name
                                 "c:\\00\\root\\gtk-ffi\\gtk-ffi"))
  :libraries nil
  :distributed-files nil
  :internally-loaded-files nil
  :project-package-name :cells-gtk
  :main-form nil
  :compilation-unit t
  :verbose nil
  :runtime-modules '(:cg-dde-utils :cg.base :cg.dialog-item :cg.timer
                     :cg.tooltip)
  :splash-file-module (make-instance 'build-module :name "")
  :icon-file-module (make-instance 'build-module :name "")
  :include-flags '(:compiler :top-level :local-name-info)
  :build-flags '(:allow-debug :purify)
  :autoload-warning t
  :full-recompile-for-runtime-conditionalizations nil
  :default-command-line-arguments "+cx +t \"Initializing\""
  :additional-build-lisp-image-arguments '(:read-init-files nil)
  :old-space-size 256000
  :new-space-size 6144
  :runtime-build-option :standard
  :on-initialization 'cells-gtk::gtk-demo
  :on-restart 'do-default-restart)

;; End of Project Definition
