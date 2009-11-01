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

(in-package :gtk-ffi)

(def-gtk-lib-functions :gtk
  ;;list-store
  (gtk-list-store-newv :pointer
		       ((n-columns gint)
			(col-types (:pointer gtype))))
  (gtk-list-store-set-value :void
			    ((store :pointer)
			     (iter :pointer)
			     (column gint)
			     (value :pointer)))
  (gtk-list-store-append :void
			 ((list-store :pointer)
			  (iter :pointer)))
  (gtk-list-store-clear :void
			((list-store :pointer)))
  ;;tre-store
  (gtk-tree-store-newv :pointer
		       ((n-columns gint)
			(col-types (:pointer gtype))))
  (gtk-tree-store-set-value :void
			    ((store :pointer)
			     (iter :pointer)
			     (column gint)
			     (value :pointer)))
  (gtk-tree-store-append :void
			 ((tree-store :pointer)
			  (iter :pointer)
			  (parent :pointer)))
  (gtk-tree-store-remove gboolean
			 ((tree-store :pointer)
			  (iter :pointer)))
  (gtk-tree-store-clear :void
			((list-store :pointer)))
  ;;tre-view
  (gtk-tree-view-new :pointer ())
  (gtk-tree-view-set-model :void
			   ((tree-view :pointer)
			    (model :pointer)))
  (gtk-tree-view-get-model :pointer
			   ((tree-view :pointer)))
  (gtk-tree-view-append-column gint
                               ((tree-view :pointer)
                                (column :pointer)))
  (gtk-tree-view-insert-column gint
			       ((tree-view
				 :pointer)
				(column :pointer)
				(pos gint)))
  (gtk-tree-view-get-selection :pointer
			       ((tree-view :pointer)))
  (gtk-tree-view-get-path-at-pos gboolean
				 ((tree-view :pointer)
				  (x gint)
				  (y gint)
				  (path :pointer)
				  (column :pointer)
				  (cell-x :pointer)
				  (cell-y :pointer)))
  (gtk-tree-view-widget-to-tree-coords :void
				       ((tree-view :pointer)
					(wx gint)
					(wy gint)
					(tx (:pointer gint))
					(ty (:pointer gint))))
  (gtk-tree-view-tree-to-widget-coords :void
				       ((tree-view :pointer)
					(wx gint)
					(wy gint)
					(tx (:pointer gint))
					(ty (:pointer gint))))
  (gtk-tree-view-expand-all :void
			    ((tree-view :pointer)))
  ;;tree-iter
  (gtk-tree-iter-free :void ((iter :pointer)))
  (gtk-tree-model-get :void
		      ((tree-model :pointer)
		       (iter :pointer)
		       (column :int)
		       (data :pointer)
		       (eof :int)))
  (gtk-tree-model-get-iter gboolean
			   ((tree-model :pointer)
			    (iter :pointer)
			    (path :pointer)))
  ;;tree-model
  (gtk-tree-model-get-iter-from-string gboolean
				       ((tree-model :pointer)
					(iter :pointer)
					(path gtk-string)))
  ;;tree-path
  (gtk-tree-path-new-from-string :pointer
				 ((path gtk-string)))
  (gtk-tree-path-to-string gtk-string
			   ((path :pointer)))
  (gtk-tree-path-free :void ((path :pointer)))
  (gtk-tree-model-get-path :pointer
			   ((tree-model :pointer)
			    (iter :pointer)));
  ;; tree-row
  (gtk-tree-row-reference-new :pointer
			      ((tree-model :pointer)
			       (path :pointer)))
  (gtk-tree-row-reference-valid gboolean
				((tree-row-reference :pointer)))
  (gtk-tree-row-reference-get-model :pointer
				    ((tree-row-reference :pointer)))
  (gtk-tree-row-reference-get-path :pointer
				   ((tree-row-reference :pointer)))
  (gtk-tree-row-reference-free :void
			       ((tree-row-reference :pointer)))
  ;;tree-selection
  (gtk-tree-selection-set-mode :void
			       ((sel :pointer)
				(mode gtk-selection-mode)))
  (gtk-tree-selection-get-mode gtk-selection-mode
			       ((sel :pointer)))
  (gtk-tree-selection-select-path :void
				  ((sel :pointer)
				   (path :pointer)))
  (gtk-tree-selection-get-selected gboolean
				   ((sel :pointer)
				    (model :pointer)
				    (iter :pointer)))
  (gtk-tree-selection-selected-foreach :void
				       ((sel :pointer)
					(callback-f :pointer)
					(data gpointer)))
  ;;tre-view-column
  (gtk-tree-view-column-new :pointer ())
  (gtk-tree-view-remove-column gint ((tree-view :pointer)
				(column :pointer)))
  (gtk-tree-view-column-pack-start :void
				   ((tree-column :pointer)
				    (renderer :pointer)
				    (expand gboolean)))
  (gtk-tree-view-column-add-attribute :void
				      ((tree-column :pointer)
				       (renderer :pointer)
				       (attribute gtk-string)
				       (column gint)))
  (gtk-tree-view-column-set-spacing :void
				    ((tree-column :pointer)
				     (spacing gint)))
  (gtk-tree-view-column-set-visible :void
				    ((tree-column :pointer)
				     (spacing gboolean)))
  (gtk-tree-view-column-set-reorderable :void
					((tree-column :pointer)
					 (resizable gboolean)))
  (gtk-tree-view-column-set-sort-column-id :void
					   ((tree-column :pointer)
					    (col-id gint)))
  (gtk-tree-view-column-set-sort-indicator :void
					   ((tree-column :pointer)
					    (resizable gboolean)))
  (gtk-tree-view-column-set-resizable :void 
				      ((tree-column :pointer)
				       (resizable gboolean)))
  (gtk-tree-view-column-set-fixed-width :void
					((tree-column :pointer)
					 (fixed-width gint)))
  (gtk-tree-view-column-set-min-width :void
				      ((tree-column :pointer)
				       (min-width gint)))
  (gtk-tree-view-column-set-max-width :void
				      ((tree-column :pointer)
				       (max-width gint)))
  (gtk-tree-view-column-set-title :void
				  ((tree-column :pointer)
				   (title :string)))
  (gtk-tree-view-column-set-expand :void
				   ((tree-column :pointer)
				    (expand gboolean)))
  (gtk-tree-view-column-set-clickable :void
				      ((tree-column :pointer)
				       (clickable gboolean)))
  (gtk-tree-view-column-set-cell-data-func :void
					   ((tree-column :pointer)
					    (cell-renderer :pointer)
					    (func :pointer)
					    (data gpointer)
					    (destroy :pointer)))
  ;;cell-renderer
  (gtk-cell-renderer-text-new :pointer ())
  (gtk-cell-renderer-toggle-new :pointer ())
  (gtk-cell-renderer-pixbuf-new :pointer ())
  (gtk-cell-renderer-set-fixed-size :void
				    ((cell :pointer)
				     (width gint)
				     (height gint))))

