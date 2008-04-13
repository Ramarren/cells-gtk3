;; -*- lisp-version: "7.0 [Windows] (Dec 28, 2004 17:34)"; cg: "1.54.2.17"; -*-

(in-package :cg-user)

(defpackage :GTK-FFI)

(define-project :name :gtk-ffi
  :modules (list (make-instance 'module :name "gtk-ffi.lisp")
                 (make-instance 'module :name "gtk-core.lisp")
                 (make-instance 'module :name "gtk-button.lisp")
                 (make-instance 'module :name "gtk-list-tree.lisp")
                 (make-instance 'module :name "gtk-menu.lisp")
                 (make-instance 'module :name "gtk-tool.lisp")
                 (make-instance 'module :name "gtk-other.lisp")
                 (make-instance 'module :name "gtk-utilities.lisp"))
  :projects (list (make-instance 'project-module :name
                                 "c:\\cell-cultures\\hello-c\\hello-c"))
  :libraries nil
  :distributed-files nil
  :internally-loaded-files nil
  :project-package-name :gtk-ffi
  :main-form nil
  :compilation-unit t
  :verbose nil
  :runtime-modules nil
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
  :on-initialization 'gtk-ffi::gtk-demo
  :on-restart 'do-default-restart)

;; End of Project Definition
