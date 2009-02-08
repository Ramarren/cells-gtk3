

;;; run gtk in its own thread (requires bordeaux-threads)
(pushnew :cells-gtk-threads *features*)

;;; drawing-area widget using cairo 
;;; (requires cl-cairo2, libgtkglext1 and libcellsgtk)
(pushnew :cells-gtk-cairo *features*)
(pushnew :cells-gtk-opengl *features*)
(pushnew :libcellsgtk *features*)


(asdf:defsystem :test-gtk
    :name "test-gtk"
  :depends-on (:cells-gtk
	       #+cells-gtk-opengl :cl-opengl
	       #+cells-gtk-opengl :cl-glu
	       #+cells-gtk-opengl :cl-glut)
  :serial t
  :components
  ((:module "test-gtk"
            :components
            ((:file "test-gtk")
             (:file "test-layout")
             (:file "test-display")
             (:file "test-buttons")
             (:file "test-entry")
             (:file "test-tree-view")
             (:file "test-menus")
             (:file "test-dialogs")
             (:file "test-textview")
             #+(or cells-gtk-opengl cells-gtk-cairo) (:file "test-drawing")
             (:file "test-addon")))))
