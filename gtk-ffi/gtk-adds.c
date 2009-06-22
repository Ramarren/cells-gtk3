#include <stdlib.h>
#include <gtk/gtk.h>
#include <glib.h>

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
/*  Return a pointer to the vbox of a dialog. 
 *  Useful for adding widgets to dialogs. For example,
 *  if you need a dialog with text entry capability. 
 */
GtkWidget *
gtk_adds_dialog_vbox (GtkWidget *dialog)
{
  return GTK_DIALOG(dialog)->vbox;
}

/*  Return a pointer to the popup_menu of a textview. 
 *  Useful if you need to add to the default textview menu
 *  on a populate-popup event. 
 */

GtkWidget *
gtk_adds_text_view_popup_menu (GtkWidget *text_view)
{
  return GTK_TEXT_VIEW(text_view)->popup_menu;
}

/* C programmers allocate iters on the stack. We use this.
   Free it with gtk-text-iter-free */
GtkTextIter *
gtk_adds_text_iter_new ()
{
  GtkTextIter example;
  return gtk_text_iter_copy(&example);
}

/* C programmers allocate iters on the stack. We use this.
   Free it with gtk-tree-iter-free */

GtkTreeIter *
gtk_adds_tree_iter_new ()
{
  GtkTreeIter example;
  return gtk_tree_iter_copy(&example);
}

int gtk_adds_widget_mapped_p (GtkWidget *wid)
{ 
    return ((GTK_WIDGET_FLAGS (wid) & GTK_MAPPED) != 0) ? 1 : 0;
}

int gtk_adds_widget_visible_p (GtkWidget *wid)
{ 
    return ((GTK_WIDGET_FLAGS (wid) & GTK_VISIBLE) != 0) ? 1 : 0;
}

GdkWindow * 
gtk_adds_widget_window (GtkWidget *wid)
{
    return wid->window;
}

GdkColor *
gtk_adds_color_new ()
{
    return ((GdkColor *)malloc(sizeof(GdkColor)));
}

void
gtk_adds_color_set_rgb (GdkColor* color, guint r, guint g, guint b)
{
    color->red = r;
    color->green = g;
    color->blue = b;
}

/* You can run this one without having gtk running, to be sure the library was loaded. */
int
gtk_adds_ok ()
{
  return 1;
}

/* This macro tells us whether g_thread_init has already been called from this session
   This is important to avoid double initialization, which kills the current lisp session */

int
gtk_adds_g_thread_supported ()
{
  return g_thread_supported ();
}

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

