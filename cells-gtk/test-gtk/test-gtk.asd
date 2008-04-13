(asdf:defsystem :test-gtk
  :name "test-gtk"
  :depends-on (:cells-gtk)
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
   (:file "test-drawing")
   (:file "test-addon")
))
