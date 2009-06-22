# 
# Purpose: build libcellsgtk.so
#
#  NOTE THAT THERE IS A libcellsgtk.so FOR LINUX AT: 
#   ftp://common-lisp.net/pub/project/cells-gtk/libcellsgtk.so
#  If you try it, I'd be interested to know if you have problems due to 
#  version mismatch with your GTK+ installation
#
#  You don't need libCellsGtk.so to run the demo, but you will to:
#   - add an entry text widget to a dialog
#   - add menu items using populate-popup (see GTK textview).
#   - Use a TreeModel (hierarchical arrangment of items) in a ComboBox.
#   - Use GTK text iters (used for marking text in text-buffers). 
#   - Use the drawing function: setting colors, getting the window of a widget
#
#  As of this writing, those are the only situations where it is needed. 
#  But this list is getting longer with each release.
#  See FAQ.txt for more of the motivation.
#
#  In order to compile the library you will need to have on hand the C header files 
#  corresponding the libgtk.so you are using. 
#  See http://developer.gnome.org/doc/API/2.4/gtk/gtk-building.html
#  On linux, it is a matter of installing 4 or 5 .rpms and typing "make"
#  Or at least that is how it worked for me.
#
#  Once built, place the library in the directory containing libgtk.

all:
	gcc -fPIC -c gtk-adds.c `pkg-config --cflags --libs gtk+-2.0`
	gcc -shared -o libcellsgtk.so gtk-adds.o `pkg-config --cflags --libs gtk+-2.0`

macports:
	gcc-mp-4.3 -fPIC -c gtk-adds.c `pkg-config --cflags --libs gtk+-2.0`
	gcc-mp-4.3 -shared -o libcellsgtk.dylib gtk-adds.o `pkg-config --cflags --libs gtk+-2.0`

solaris:
	gcc -fPIC -R/opt/csw/lib -L/opt/csw/lib -c gtk-adds.c `pkg-config --cflags --libs gtk+-2.0 glib-2.0`
	gcc -fPIC -R/opt/csw/lib -L/opt/csw/lib -shared -o libcellsgtk.so gtk-adds.o `pkg-config --cflags --libs gtk+-2.0 glib-2.0`

