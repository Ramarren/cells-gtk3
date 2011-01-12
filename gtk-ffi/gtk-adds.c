#include <stdlib.h>
#include <gtk/gtk.h>
#include <glib.h>

#ifdef WIN32 // .drectve is an win32 extension
asm (".section .drectve");
asm (".ascii \"-export:gtk_adds_text_iter_new\"");
asm (".ascii \" -export:gtk_adds_text_view_popup_menu\"");
asm (".ascii \" -export:gtk_adds_dialog_vbox\"");
asm (".ascii \" -export:gtk_adds_tree_iter_new\"");
asm (".ascii \" -export:gtk_adds_widget_mapped_p\"");
asm (".ascii \" -export:gtk_adds_widget_visible_p\"");
asm (".ascii \" -export:gtk_adds_widget_window\"");
asm (".ascii \" -export:gtk_adds_color_new\"");
asm (".ascii \" -export:gtk_adds_color_set_rgb\"");
asm (".ascii \" -export:gtk_adds_ok\"");
asm (".ascii \" -export:gtk_adds_g_thread_supported\"");
asm (".ascii \" -export:gtk_adds_widget_height\"");
asm (".ascii \" -export:gtk_adds_widget_width\"");
#endif

/* This is to return the new allocated height/width after the user reshapes a widget */
int
gtk_adds_widget_height (GtkWidget *wid)
{
  return wid->allocation.height;
}

int
gtk_adds_widget_width (GtkWidget *wid)
{
  return wid->allocation.width;
}

