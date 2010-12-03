(load (merge-pathnames "features.lisp" *load-truename*))

(in-package "CL-USER")

(asdf:defsystem :gtk-ffi
    :name "gtk-ffi"
  :depends-on (:cells
	       :pod-utils
	       :cffi
               :trivial-features
	       :utils-kt
	       #+cells-gtk-threads :bordeaux-threads
	       )
  :components
  ((:module "gtk-ffi"
            :components
            ((:file "package")
             (:file "glibraries" :depends-on ("package"))
             (:file "gtypes" :depends-on ("package"))
             (:file "gtk-ffi-impl" :depends-on ("package"))
             (:file "gtk-threads" :depends-on ("package"))
             (:file "gtk-ffi" :depends-on ("gtk-threads" "gtk-ffi-impl" "gtypes" "glibraries"))
             #+cells-gtk-opengl (:file "gtk-gl-ext" :depends-on ("package" "gtk-ffi" "gtypes" "glibraries"))
             (:file "gtk-core" :depends-on ("gtk-ffi" "gtypes" "glibraries"))
             (:file "gtk-other" :depends-on ("gtk-ffi" "gtypes" "glibraries"))
             (:file "gtk-button" :depends-on ("gtk-ffi" "gtypes" "glibraries"))
             (:file "gtk-tool" :depends-on ("gtk-ffi" "gtypes" "glibraries"))
             (:file "gtk-menu" :depends-on ("gtk-ffi" "gtypes" "glibraries"))
             (:file "gtk-list-tree" :depends-on ("gtk-ffi" "gtypes" "glibraries"))
             (:file "gtk-utilities" :depends-on ("gtk-core" "gtk-other" "gtk-list-tree" "gtypes" "glibraries"))))))
