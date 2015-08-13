# Release notes #

### Released Syx 0.1 version ###

This is the first version of Syx which includes an almost stable VM without memory leaks till now (we hope).
  * Added a simple garbage collector.
  * Covered a limited number of Smalltalk-80 standards.
  * Image compatible with both 32-bit and 64-bit platforms.
  * The VM indifferently works on Win32 and Posix.
  * Use Scons as building system
  * Interactive console from within Smalltalk itself
  * Immediate SmallIntegers and Characters
  * A basic plugin system
  * Support for Floats and LargeIntegers
  * Fixed operations between different numbers
  * Easy C to Smalltalk interaction
  * Added class variables
  * Variable bindings for globals and class variables
  * Added OrderedCollection
  * Very simple method caching
  * Still limited exceptions and tracebacks
  * Remove Glib dependency
  * Synchronous I/O multi-plexing through Semaphores
  * Use direct pointers instead of indexes for oops
  * Better organized debugging informations
  * Documented using Doxygen
  * Reorganized Syx initialization

See the [ChangeLog](http://syx.googlecode.com/svn/trunk/ChangeLog) for a complete list of things that changed.