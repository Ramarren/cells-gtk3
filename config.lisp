(in-package :cl-user)

#+lispworks
(setq *HANDLE-EXISTING-DEFPACKAGE* '(:modify))


#| 


 ---->  THIS ISN'T USED IF YOU ARE LOADING A SNAPSHOT TARBALL. <-----

  Tell ASDF where to find everything. 

|#


(push (make-pathname :directory '(:absolute "local" "lisp" "cells-gtk" "utils-kt"))
  asdf:*central-registry*)
  
(push (make-pathname :directory '(:absolute "local" "lisp" "cells-gtk" "cells"))
  asdf:*central-registry*)
  
(push (make-pathname :directory '(:absolute "local" "lisp" "cells-gtk" "hello-c"))
  asdf:*central-registry*)
  
(push (merge-pathnames
       (make-pathname :directory '(:relative "gtk-ffi"))
       *load-pathname*)
  asdf:*central-registry*)

(push (merge-pathnames
       (make-pathname :directory '(:relative "cells-gtk"))
       *load-pathname*)
  asdf:*central-registry*)

#| --------------------------------------
   Add test-gtk, and tell it where to find images
|#

(push (merge-pathnames
       (make-pathname :directory '(:relative "cells-gtk" "test-gtk"))
       *load-pathname*)
  asdf:*central-registry*)

