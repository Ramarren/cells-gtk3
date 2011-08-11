#|

 Gtk ffi

 Copyright (c) 2004 by Vasilis Margioulas <vasilism@sch.gr>

 You have the right to distribute and use this software as governed by 
 the terms of the Lisp Lesser GNU Public License (LLGPL):

    (http://opensource.franz.com/preamble.html)
 
 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 Lisp Lesser GNU Public License for more details.
 
|#

;;; Function with equivalents in gtklib.

(in-package :gtk-ffi)

(cffi:defcenum g-connect-flags
  (:none 0)
  (:after 1)
  (:swapped 2)
  (:after-swapped 3))

(cffi:defcfun ("g_signal_connect_data" g_signal_connect_data) gulong
  (instance :pointer)
  (detailed-signal :pointer)
  (c-handler :pointer) 
  (data :pointer)
  (destroy-data :pointer)
  (connct-flags g-connect-flags))

(defun g-signal-connect-data (self detailed-signal c-handler data destroy-data after)
  (cffi:with-foreign-string (c-detailed-signal detailed-signal)
    (let ((p4 (or data +c-null+)))
      (g_signal_connect_data
       self
       c-detailed-signal
       (wrap-func c-handler)
       p4
       (or destroy-data +c-null+)
       (if after :after :none)))))

(defun gtk-signal-connect (widget signal fun &key (after t) data destroy-data)
  #+shhtk (print (list "passing fun to gtk-signal-connect" signal fun))
  (g-signal-connect-data widget signal fun data destroy-data after))

(defun wrap-func (func-address) ;; vestigial. func would never be nil. i think.
  (or func-address 0))

(defun gtk-signal-connect-swap (widget signal fun &key (after t) (data +c-null+) (destroy-data +c-null+)) ; pod 0216
  (g-signal-connect-closure widget signal
    (g-cclosure-new-swap (wrap-func fun) data destroy-data) after))

(defun gtk-object-set-property (obj property val-type val)
  (with-g-value (value)
    (g-value-init value (value-type-as-int val-type))
    (funcall (value-set-function val-type)
             value val)
    (g-object-set-property obj property value)
    (g-value-unset value)))

(defun get-gtk-string (pointer)
  (typecase pointer
    (string pointer)
    (otherwise
     (cffi:mem-ref pointer 'gtk-string))))

(cffi:defcallback button-press-event-handler :int
                  ((widget :pointer) (signal :pointer) (data :pointer))
  (declare (ignore data))
  (let ((event (gdk-event-button-type signal)))
    (when (or (eql event :button_press)
              (eql event :button_release))
      (when (= (gdk-event-button-button signal) 3)
        (gtk-menu-popup widget +c-null+ +c-null+ +c-null+ +c-null+ 3
			(gdk-event-button-time signal)))))
  0)

(defun gtk-widget-set-popup (widget menu)
  (gtk-signal-connect-swap widget "button-press-event"
                           (cffi:get-callback 'button-press-event-handler)
                           :data menu)
  (gtk-signal-connect-swap widget "button-release-event"
                           (cffi:get-callback 'button-press-event-handler)
                           :data menu))

(defun gtk-list-store-new (col-types)
  (cffi:with-foreign-object (c-types 'gtype (length col-types))
    (loop for type in col-types
          for n upfrom 0
          do (setf (cffi:mem-aref c-types 'gtype n) (coerce (as-gtk-type type) 'integer)))
    (gtk-list-store-newv (length col-types) c-types)))


(defun gvi (&optional (key :anon))
  key)

(defun gtk-list-store-set (lstore iter types-lst data-lst)
  (with-g-value (value)
    (gvi :with-type-val)
    (loop for col from 0
          for data in data-lst
          for type in types-lst
          do
       (gvi :pre-truvi)
       (g-value-init value (as-gtk-type type))
          #+msg (format t "~&gtk-list-store-set:  setting value to ~A~%" data)
                (funcall (intern (format nil "G-VALUE-SET-~a" (case type
                                                                (:date 'float)
                                                                (:icon 'string)
                                                                (t type)))
                                 :gtk-ffi)
                         value
                         (or (and (eql type :date)
                                  (coerce data 'single-float))
                             data))
                (gtk-list-store-set-value lstore iter col value)
                (g-value-unset value)
          #+not (when str-ptr (uffi:free-cstring str-ptr)))))

(defun gtk-list-store-set-items (store types-lst data-lst)
  (with-tree-iter (iter)
    (dolist (item data-lst)
      (gvi :pre-append)
      (gtk-list-store-append store iter)
      (gvi :pre-set)
      (gtk-list-store-set store iter types-lst item)
      (gvi :post-set))))

(defun gtk-tree-store-new (col-types)
  (cffi:with-foreign-object (c-types 'gtype (length col-types))
    (loop for type in col-types
          for n upfrom 0
          do (setf (cffi:mem-aref c-types 'gtype n) (coerce (as-gtk-type type) 'integer)))
    (gtk-tree-store-newv (length col-types) c-types)))

(defun gtk-tree-store-set-cell (tstore iter col type data)
  "Sets the value of one cell in a row referenced by iter and a col referenced by col."
  (with-g-value (value)
    (g-value-init value (as-gtk-type type))
    (funcall (case type
	       ((:string :icon) #'g-value-set-string)
	       (:int #'g-value-set-int)
	       (:long #'g-value-set-long)
	       (:boolean #'g-value-set-boolean)
	       ((:float :date) #'g-value-set-float)
	       (t (error "Invalid type: ~S?" type)))
	     value
	     (if (eql type :date) (coerce data 'single-float) data))
    (gtk-tree-store-set-value tstore iter col value)
    (g-value-unset value)))

(defun gtk-tree-store-set (tstore iter types-lst data-lst)
  "Sets the value of one or more cells in a row referenced by iter."
  (loop for col from 0
     for data in data-lst
     for type in types-lst
     do ;; (print (list :tree-store-set value type (as-gtk-type type)))
       (gtk-tree-store-set-cell tstore iter col type data)))

;;; todo: The deref-pointer-runtime-typed used by case needs work if
;;; it is going to be used for lispworks, cmu and allegro.
;;; (needs someone who knows how ffi-to-uffi-type maps types for those lisps.)
;;; Even better, eliminate it. It is ill-conceived.
(defun gtk-tree-model-get-cell (model iter column-no cell-type)
  "Returns the item at column-no if column-no [0,<num-columns-1>] or a
   a string like '(0 1 0)', which navigates to the selected item, if 
   column-no = num-columns. (See gtk-tree-store-set-kids)."
  (cffi:with-foreign-object (item :pointer)
    (gtk-tree-model-get model iter column-no item -1)
    (case cell-type
      (:string (cffi:mem-ref item 'gtk-string))
      (t (cffi:mem-ref item (as-gtk-type-name cell-type))))))

(defun parse-cell-attrib (attribs)
  (loop for (attrib val) on attribs by #'cddr collect
	(ecase attrib
	  (:foreground (list "foreground" 'gtk-string val))
	  (:background (list "background" 'gtk-string val))
	  (:font (list "font" 'gtk-string val))
	  (:size (list "size-points" 'gdouble (coerce val 'double-float)))
	  (:strikethrough (list "strikethrough" 'gboolean val))
	  (:editable (list "editable" 'gboolean val))
	  (:activatable (list "activatable" 'gboolean val))
	  (:radio (list "radio" 'gboolean val)))))

(defun gtk-cell-renderer-set-attribs (cell-renderer attribs)
  "Set the properties of cell-render according to attribs"
  (loop for property in (parse-cell-attrib attribs)
	   do (apply #'gtk-object-set-property cell-renderer property)))

(defun gtk-tree-model-get-typed-item-value (model iter col col-type)
  (cffi:with-foreign-object (return-buffer :int 16)
    (gtk-tree-model-get model iter col return-buffer -1)
    (cffi:mem-ref return-buffer (as-gtk-type-name col-type))))

;;; These seem both overly convoluted, not used in other code and not exported. Remove?
(progn
  (defun alloc-col-type-buffer (col-type)
    (ecase col-type
      ((:string :icon) (cffi:foreign-alloc '(:array :pointer 1)))
      (:boolean (cffi:foreign-alloc '(:array :unsigned-char 1))) ;;guess
      (:date (cffi:foreign-alloc '(:array :float 1)))
      (:int (cffi:foreign-alloc '(:array :int 1)))
      (:long (cffi:foreign-alloc '(:array :long 1)))
      (:float (cffi:foreign-alloc '(:array :float 1)))
      (:double (cffi:foreign-alloc '(:array :double 1)))))
  
  (defun deref-col-type-buffer (col-type buffer)
    (ecase col-type
      ((:string :icon)
       (get-gtk-string (cffi:make-pointer (cffi-sys:pointer-address (cffi:mem-aref buffer :pointer 0)))))
      (:boolean (not (zerop (cffi:mem-aref buffer :unsigned-char 0))))
      (:date (cffi:mem-aref buffer :FLOAT 0))
      (:int (cffi:mem-aref buffer :int 0))
      (:long (cffi:mem-aref buffer :long 0))
      (:float (cffi:mem-aref buffer :float 0))
      (:double (cffi:mem-aref buffer :double 0)))))




(defun gtk-file-chooser-get-filenames-strs (file-chooser)
  (let ((glist (gtk-file-chooser-get-filenames file-chooser)))
    (loop for lst-address = glist then (cffi:foreign-slot-value lst-address 'gslist 'next)
        while (and lst-address (not (cffi:null-pointer-p lst-address)))
        collect (cffi:foreign-slot-value lst-address 'gslist 'data)
        finally (g-slist-free glist))))


