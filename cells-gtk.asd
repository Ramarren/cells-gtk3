(load (merge-pathnames "features.lisp" *load-truename*))

(asdf:defsystem :cells-gtk
    :name "cells-gtk"
  :depends-on (:cells
	       :utils-kt
	       :pod-utils
	       :gtk-ffi
	       :ph-maths
	       #+cells-gtk-cairo :cl-cairo2
	       #+(and cells-gtk-cairo unix) :cl-cairo2-xlib
	       #+(and cells-gtk-cairo win32) :cl-cairo2-win32
	       #+cells-gtk-threads :bordeaux-threads
	       #+cells-gtk-opengl :cl-opengl
	       #+cells-gtk-opengl :cl-glu
	       #+cells-gtk-opengl :cl-glut)
  :serial t
  :components
  ((:module "cells-gtk"
            :serial t
            :components
            ((:file "packages")   
             (:file "conditions")
             (:file "compat")
             (:file "cells3-porting-notes" :depends-on ("packages"))
             (:file "widgets" :depends-on ("conditions"))
             (:file "layout" :depends-on ("widgets"))
             (:file "display" :depends-on ("widgets"))
             (:file "drawing-area" :depends-on ("widgets"))
             #+cells-gtk-cairo (:file "cairo-drawing-area" :depends-on ("widgets"))
             #+cells-gtk-opengl (:file "gl-drawing-area" :depends-on ("widgets"))
             (:file "buttons" :depends-on ("widgets"))
             (:file "entry" :depends-on ("widgets"))
             (:file "tree-view" :depends-on ("widgets"))
             (:file "menus" :depends-on ("widgets"))
             (:file "dialogs" :depends-on ("widgets"))
             (:file "textview" :depends-on ("widgets"))
             (:file "addon" :depends-on ("widgets"))
             (:file "gtk-app")))))
