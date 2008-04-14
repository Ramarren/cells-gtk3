

;;; run gtk in its own thread (requires bordeaux-threads)
(pushnew :cells-gtk-threads *features*)

;;; drawing-area widget using cairo (requires cl-cairo2)
(pushnew :cells-gtk-cairo *features*)

;;; drawing-area widget using OpenGL (requires libgtkglext1)
(pushnew :cells-gtk-opengl *features*)


(asdf:defsystem :test-gtk
  :name "test-gtk"
  :depends-on (:cells-gtk
	       #+cells-gtk-opengl :cl-opengl
	       #+cells-gtk-opengl :cl-glu
	       #+cells-gtk-opengl :cl-glut)
  :serial t
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
   (:file "test-addon")
))
