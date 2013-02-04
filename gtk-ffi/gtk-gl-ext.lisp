
#|

This is taken from

(defsystem :gtkgl
    :depends-on (:cffi)
    :name "GTKGL"
    :author "Dmitri Hrapof <hrapof@common-lisp.ru>"
    :maintainer "Dmitri Hrapof <hrapof@common-lisp.ru>"
    :licence "LGPL"
    :description "CFFI bindings for GtkGlExt"
    :components ((:file "gtk-gl-ext")))
|#

(in-package :gtk-ffi)

;(export '(with-gl-drawable with-swap-buffers))

(cffi:define-foreign-library libgtkglext
  (:darwin "libgtkglext-x11-1.0.dylib")
  (:unix (:or "libgtkglext-x11-1.0.so" "libgtkglext-x11-1.0.so.0"))
  (:windows "libgtkglext-win32-1.0-0.dll")
  (t (:default "libgtkglext")))
   
(cffi:use-foreign-library libgtkglext)

(cffi:define-foreign-library libgdkglext
  (:darwin "libgdkglext-x11-1.0.dylib")
  (:unix (:or "libgdkglext-x11-1.0.so" "libgdkglext-x11-1.0.so.0"))
  (:windows "libgdkglext-win32-1.0-0.dll")
  (t (:default "libgdkglext")))

(cffi:use-foreign-library libgdkglext)

(cffi:defbitfield gdk-gl-config-mode
  (:gdk-gl-mode-rgba 0)
  :gdk-gl-mode-index
  :gdk-gl-mode-double
  :gdk-gl-mode-stereo
  :gdk-gl-mode-alpha
  :gdk-gl-mode-depth
  :gdk-gl-mode-stencil
  :gdk-gl-mode-accum
  :gdk-gl-mode-multisample)

(cffi:defcenum gdk-gl-render-type
    (:gdk-gl-rgba-type #x8014)
    (:gdk-gl-color-index-type #x8015))

(def-gtk-lib-functions :gdkglext
  (gdk-gl-config-new-by-mode :pointer
			     ((mode gdk-gl-config-mode)))
  (gdk-gl-config-new-by-mode-for-screen :pointer
					((screen :pointer)
					 (mode gdk-gl-config-mode)))
  (gdk-gl-config-is-rgba gboolean
			 ((glconfig :pointer)))
  (gdk-gl-config-is-double-buffered gboolean
				    ((glconfig :pointer)))
  (gdk-gl-context-new :pointer
		      ((drawable :pointer)
		       (share-list :pointer)
		       (direct gboolean)
		       (render-type gdk-gl-render-type)))
  (gdk-gl-context-destroy :void
			  ((context :pointer)))
  (gdk-gl-window-new :pointer
		     ((glconfig :pointer)
		     
		      (window :pointer)
		      (attrib-list :pointer)))
  (gdk-gl-window-destroy :void
			 ((window :pointer)))
  (gdk-gl-drawable-gl-begin gboolean
			    ((gldrawable :pointer)
			     (glcontext :pointer)))
  (gdk-gl-drawable-gl-end :void
			  ((gldrawable :pointer)))
  (gdk-gl-drawable-is-double-buffered gboolean
				      ((gldrawable :pointer)))
  (gdk-gl-drawable-swap-buffers :void
				((gldrawable :pointer))))

(def-gtk-lib-functions :gtkglext
  (gtk-gl-init :void
		 ((argc :pointer)
		  (argv :pointer)))
  (gtk-gl-init-check gboolean
		     ((argc :pointer)
		      (argv :pointer)))
  (gtk-widget-set-gl-capability gboolean
				((widget :pointer)
				 (glconfig :pointer)
				 (share-list :pointer)
				 (direct gboolean)
				 (render-type gdk-gl-render-type)))
  (gtk-widget-get-gl-context :pointer
			     ((widget :pointer)))
  (gtk-widget-get-gl-window :pointer
			    ((widget :pointer))))

(defmacro with-gl-drawable (widget &rest body)
  (let ((context (gensym)) (drawable (gensym)))
   `(let ((,context (gtk-widget-get-gl-context ,widget))
	  (,drawable (gtk-widget-get-gl-window ,widget)))
      (gdk-gl-drawable-gl-begin ,drawable ,context)
      (progn ,@body)
      (gdk-gl-drawable-gl-end ,drawable))))

(defmacro with-swap-buffers (widget &rest body)
  (let ((context (gensym)) (drawable (gensym)))
   `(let ((,context (gtk-widget-get-gl-context ,widget))
	  (,drawable (gtk-widget-get-gl-window ,widget)))
      (gdk-gl-drawable-gl-begin ,drawable ,context)
      (progn ,@body)
      (if (gdk-gl-drawable-is-double-buffered ,drawable)
	  (gdk-gl-drawable-swap-buffers ,drawable))
      (gdk-gl-drawable-gl-end ,drawable))))


