(in-package :cl)

;;;
;;; features
;;;

;;; I am not Kenny
(pushnew :iamnotkenny *features*)

;;; run gtk in its own thread (requires bordeaux-threads)
(pushnew :cells-gtk-threads *features*)

;;; drawing-area widget using cairo (requires cl-cairo2)
(pushnew :cells-gtk-cairo *features*)

;;; drawing-area widget using OpenGL (requires libgtkglext1)
(pushnew :cells-gtk-opengl *features*)

;;; gtype with size
(pushnew :cells-gtk-gtype-is-gulong *features*)

;;; library path for MacPorts
#+darwin (progn
           (asdf:oos 'asdf:load-op :cffi)
           (pushnew "/opt/local/lib/" (symbol-value (find-symbol "*FOREIGN-LIBRARY-DIRECTORIES*"
                                                                 (find-package "CFFI")))
                    :test #'string=))
