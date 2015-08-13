# Release notes #

### Released Syx 0.1.4 version ###

A more stable VM has been reached with this version. Many features have been added for both the embedding and the environment. This version also includes the "image endianness compatibility" target of the 0.2 roadmap and a lot of wrapper methods for GTK+ with a basic GtkWorkspace.
This is the list of the major changes:
  * Running blocking processes inside the scheduler has been fixed
  * Major fixes to the garbage collector, object finalization, number parsing and to the scheduler
  * Rehashing support for hashed collections such as Dictinoary, Set and Bag
  * #perform: and #copyReplace family ANSI methods have been added
  * Interval and StdIOStream classes have been added
  * Missing lexer rules for quoted symbols and double quotes in strings have been added
  * ZeroDivide exception check for number division
  * Comparing methods to String have been added
  * A bunch of improvements for the SyxGtk plugin
  * Rehash dictionaries from C
  * Visual Studio compilation and debugging support
  * Useful functions for embedding Syx have been added
  * Reorganized inline functions to support other compilers and use dllexport and dllimport when building the library on Windows
  * Inheritance for method and block objects has been fixed
  * rootdir and plugindir Scons options have been added
  * Install dependent-machine object files into libdir instead of sharedir
  * Option -c has been added to syx to continue the startup sequence once filed in files from the command line
  * Support for big endian machines has been completed
  * System signal handling has been added
  * Print a bug report to the stdout on SIGSEGV or SIGILL containing informations on the memory state, the execution state and the traceback of the active broken Process
  * Image recovery on SIGSEGV and SIGILL or through the syx --recovery option
  * Scripting header (e.g. #!/usr/bin/env syx) support on fileIn
  * argumentCount check for methods and blocks has been introduced
  * Documentation installation and uninstallation is now supported
  * Support for 'a' and 'w+' mode with O\_CREAT flag have been added to FileStream
  * Several issues have been fixed for packaging binary distributions of Syx using Scons
  * Getopt dependency has been removed
  * Desktop files for Posix systems have been added

Syx changed its logo since previous version. The Windows installer will handle .st and .sim files.
Roadmap for 0.5 version includes both internal and OS-level threads based schedulers, with possibly parallel computing support.

See the [ChangeLog](http://syx.googlecode.com/svn/tags/syx-0.1.4/ChangeLog) for a complete list of things that changed.