# Frequently asked questions #

## Why doesn't Scons install files under $prefix? ##

If you are creating a binary package for Syx, the behavior is not the same as other software using mainstream building tools such as GNU Make.
I would invite you to follow the [Syx packaging instructions](PackagingInstructions.md).

Instead if you are not going to create such package, [please contact me](mailto:lethalman88@gmail.com) or subscribe to the [discussion mailing list](http://groups.google.com/group/syx-discuss)

## Why use #printNl instead of Transcript? ##

The `#printNl` method is just a faster way to call `Transcript showCr: object printString`.

## Is Syx available only on 64-bit platforms? ##

**No, it's very portable and runs on many platforms including 32-bit platforms**. You see mostly 64-bit packages in the homepage and downloads because me and other contributors work on 64-bit system so we can create only 64-bit binary packages.

This doesn't mean you can't get Syx sources, compile it for your platform and run it successfully.

## Why the configure script can't find any x11.pc on Solaris? ##

This might seem weird but it's a bug of Solaris. It's missing the x11.pc file which is the configuration file for the X11 library which is required to compile the X11 plugin of Smalltalk YX.
If you want to disable the plugin, you can issue the configure script as follows:
```
./configure --disable-x11 ...
```
If you want to install the X11 plugin, please [read the issue #27](http://code.google.com/p/syx/issues/detail?id=27) for more informations.