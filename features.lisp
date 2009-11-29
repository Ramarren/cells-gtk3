(in-package :cl)

;;; GTK fails on SBCL with FLOATING-POINT-INVALID-OPERATION trap enabled on Linux

#+(and sbcl
       ieee-floating-point
       (not darwin))
(sb-int:set-floating-point-modes
 :traps (remove :invalid (getf (sb-int:get-floating-point-modes) :traps)))

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

;;;---------------------
;;; Two steps:
;;;---------------------

;;; Step 1 -- Do nothing. (UFFI/Hello-C had a "Step 1," CFFI doesn't.)

;;; Step 2 -- If you built or downloaded the libcellsgtk library, uncomment the next line.

;;; features
(pushnew :libcellsgtk *features*)

;;; library path for MacPorts
#+darwin (progn
           (asdf:oos 'asdf:load-op :cffi)
           (pushnew "/opt/local/lib/" (symbol-value (find-symbol "*FOREIGN-LIBRARY-DIRECTORIES*"
                                                                 (find-package "CFFI")))
                    :test #'string=))
