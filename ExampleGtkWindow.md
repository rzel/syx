# Introduction #

_The GTK+ plugin is installed by default, anyway if you installed the GTK+ libraries after you installed Syx, you'll need to recompile Syx with the support of the GTK+ plugin._

Before you continue reading this very short tutorial, make sure that your copy of Smalltalk YX has been compiled with the GTK+ plugin support. If not, recompile Syx as shown below:
```
./configure --enable-gtk
make
make install
```

Or if you like to use SCons:

```
scons GTK=yes
```

Then install with:
```
scons install
```

**NB**: _this plugin doesn't work very well under Windows yet_

# Load the plugin #

The GTK+ plugin has been created using a non-official Swig module to let Syx interface with the C library.
Since it's a plugin you need to load it. The name of the plugin is **gtk**.
Well, open you syx interpreter and type this command:
```
Smalltalk loadPlugin: 'gtk'!
```

If the return value is **true** the module has been loaded successfully, if **false** you need to recompile Syx with the GTK+ plugin support as described above.

# Open the window #

Now let's open our graphical window and see what happens:
```
GtkWindow new showAll.
Gtk main!
```

You can see a window opened. Also notice that you can still use the console, because the main loop is threaded.

## Explanation ##

To create a GtkWindow you need just the following command:
```
GtkWindow new
```

But, creating it won't mean showing it, so we had to call the **showAll** method on it.

Finally, we had to start the GTK+ main loop with `Gtk main`

# Show a label #

**If you restarted Syx, be sure to load the GTK+ plugin as explained above.**

We know how to create a Window but not how to add something cool into it.
So let's add a simple label and show it:
```
| window label |
label := GtkLabel new: 'Hello world'.
window := GtkWindow new.
window add: label; showAll!
```

**If you restarted Syx, now you have to run the GTK+ main loop as shown above.**

If you want a one-row version of that, read:
```
GtkWindow new add: (GtkLabel new: 'Hello world'); showAll!
```