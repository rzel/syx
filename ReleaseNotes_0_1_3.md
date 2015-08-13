# Release notes #

### Released Syx 0.1.3 version ###

This release enhances other platforms support (Windows CE), a bunch of bug fixes, and many features like the ST compiler. This is a list of what changed since previous release:
  * Fractions, division methods and some other number functions (floor, ceiling, quo, trunc, ...) have been added
  * Some lexer-related issues (quotes, minus, ...) have been fixed
  * Fixed memory leak occurring while finalizing objects
  * Many ANSI methods for Character have been covered
  * First attempt to reach endianness image compatibility
  * String to Number methods and vice versa have been added
  * Super messages have been fixed
  * Syntax shortcuts such as Object>>#caseOf:otherwise: have been added
  * Fixed cascading parsing
  * A complete Smalltalk compiler has been added
  * Support for cross-compiling and other building options have been added to Scons
  * Save text of methods
  * Increased initial memory size from 20000 to 100000 objects
  * More C functions have been documented
  * An important issue in ArrayedCollection>>#replace:From:To:With: primitive has been fixed
  * Access mode (read, write, read and write) to Streams and more methods to FileStreams have been added
  * CompiledMethod and CompiledBlock inheritance has been reorganized
  * Run Smalltalk scripts from command line
  * New plugin to wrap native Win API for creating a simple WinWorkspace
  * Build Syx for Windows CE with WinWorkspace running on the device
  * Support for user-defined startup sequence has been added
  * Support for wide-character strings has been added
  * getopt, errno, fstat, access, getenv and perror are now optional functions


_Cross-compiling to Windows CE has been done with host=arm-wince-mingw32ce using CeGCC by disabling all configuration options.
Endianness compatibility is not perfectly reached yet._

See the [ChangeLog](http://syx.googlecode.com/svn/tags/syx-0.1.3/ChangeLog) for a complete list of things that changed.