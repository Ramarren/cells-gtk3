;; -*- lisp-version: "7.0 [Windows] (Dec 28, 2004 17:34)"; cg: "1.54.2.17"; -*-

(in-package :cg-user)

(defpackage :TEST-GTK)

(define-project :name :test-gtk
  :modules (list (make-instance 'module :name "test-gtk.lisp")
                 (make-instance 'module :name "test-layout.lisp")
                 (make-instance 'module :name "test-display.lisp")
                 (make-instance 'module :name "test-buttons.lisp")
                 (make-instance 'module :name "test-entry.lisp")
                 (make-instance 'module :name "test-tree-view.lisp")
                 (make-instance 'module :name "test-menus.lisp")
                 (make-instance 'module :name "test-dialogs.lisp")
                 (make-instance 'module :name "test-textview.lisp")
                 (make-instance 'module :name "test-addon.lisp"))
  :projects (list (make-instance 'project-module :name
                                 "c:\\00\\root\\cells-gtk\\cells-gtk"))
  :libraries nil
  :distributed-files nil
  :internally-loaded-files nil
  :project-package-name :test-gtk
  :main-form nil
  :compilation-unit t
  :verbose nil
  :runtime-modules '(:cg-dde-utils :cg.base :cg.dialog-item :cg.timer
                     :cg.tooltip)
  :splash-file-module (make-instance 'build-module :name "")
  :icon-file-module (make-instance 'build-module :name "")
  :include-flags '(:local-name-info)
  :build-flags '(:allow-debug :purify)
  :autoload-warning t
  :full-recompile-for-runtime-conditionalizations nil
  :default-command-line-arguments "+cx +t \"Initializing\""
  :additional-build-lisp-image-arguments '(:read-init-files nil)
  :old-space-size 256000
  :new-space-size 6144
  :runtime-build-option :standard
  :on-initialization 'test-gtk::gtk-demo
  :on-restart 'do-default-restart)

;; End of Project Definition
