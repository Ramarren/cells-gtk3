
(in-package "CL-USER")
;;;---------------------
;;; Two steps:
;;;---------------------

;;; Step 1 -- Do nothing. (UFFI/Hello-C had a "Step 1," CFFI doesn't.)

;;; Step 2 -- If you built or downloaded the libcellsgtk library, uncomment the next line.

;;; features
(pushnew :libcellsgtk *features*)

(asdf:defsystem :gtk-ffi
  :name "gtk-ffi"
  :depends-on (:cells
	       :pod-utils
	       :cffi
	       :cffi-uffi-compat
	       :utils-kt
	       #+cells-gtk-threads :bordeaux-threads
	       )
  :components
  ((:file "package")
   (:file "gtk-ffi-impl" :depends-on ("package"))
   (:file "gtk-threads" :depends-on ("package"))
   (:file "gtk-ffi" :depends-on ("gtk-threads" "gtk-ffi-impl"))
   #+cells-gtk-opengl (:file "gtk-gl-ext" :depends-on ("package" "gtk-ffi"))
   (:file "gtk-core" :depends-on ("gtk-ffi"))
   (:file "gtk-other" :depends-on ("gtk-ffi"))
   (:file "gtk-button" :depends-on ("gtk-ffi"))
   (:file "gtk-tool" :depends-on ("gtk-ffi"))
   (:file "gtk-menu" :depends-on ("gtk-ffi"))
   (:file "gtk-list-tree" :depends-on ("gtk-ffi"))
   (:file "gtk-utilities" :depends-on ("gtk-core" "gtk-other" "gtk-list-tree"))))
