# cells-gtk3

This is a fork of [cells-gtk3](http://common-lisp.net/project/cells-gtk/), which is a binding for GTK+ widget toolkit using Cells3. I have not substantially modified it, mostly made it run with current versions of dependencies and modified the FFI layer to use type synonyms.

# Dependencies

The system builds against version of dependencies in [Quicklisp](http://www.quicklisp.org/). Quicklisp contains all Lisp side dependencies. Obviously, you need GTK+ installed (tested with gtk+-2.20.1), and whatever other extensions you want (mostly [gtkglext](http://gtkglext.sourceforge.net/)).

### Required
- [cells](http://github.com/Ramarren/cells/tree/master)
- [cffi](http://common-lisp.net/project/cffi/)

### Optional
- [bordeaux-threads](http://common-lisp.net/project/bordeaux-threads/)
- [cl-cairo2](http://github.com/tpapp/cl-cairo2/tree/master)
- [cl-opengl](http://common-lisp.net/project/cl-opengl/)

# Installation

Note that both cells and cells-gtk3 have multiple `.asd` files in top level directory. All must be linked into central registry. 

By default are optional dependencies are turned on. If you want optional components not to be loaded you have to edit the `features.lisp` file in cells-gtk3 directory, and comment out appropriate `(pushnew ...)` command.

# Documentation

Cells themselves are documented in [cells-doc](http://github.com/stefano/cells-doc/tree/master). There is no documentation for cells-gtk3, but there is comprehensive example application in test-gtk subdirectory.

# Status

I cleaned up the library just in case I needed GUI in Lisp, but it turned out that I did not. Hence, the primary extent of my testing is running test-gtk:gtk-demo application. Bug reports and/or patches are welcome.

From my testing, the example application runs on:
### x86 linux
- SBCL 1.0.45 (tested with threading, it has been reported on the mailing list that 1.0.20 doesn't work)
- CLISP 2.47-r2
- ECL 9.12.3 (with threads and unicode)

### Intel MacOSX
 I tried only X11 GTK+ from MacPorts. It should be possible to use Quartz based version of GTK just by changing library names, but I have not tried it. There is not GTK-GL extension for Quartz.

- SBCL 1.0.30 (from MacPorts)
- CCL 1.3 (from MacPorts)
