# Release notes #

### Released Syx 0.1.2 version ###

This version has a bunch of new features added to the Smalltalk environment, and a more stable VM. Major enhancements includes:
  * New compiler flags to build Syx on Mac OS
  * Added Sets and Bags
  * Refactored objects to hold both instance variables and indexed variables
  * Literals in methods are now constant
  * Added basic WriteStream
  * Doubled initial memory size to hold 20000 objects
  * Fixed ByteArray/String small integer to character conversion
  * Added object finalization by having fixed GC transactions
  * Added support for infinite-precision numbers using GMP
  * Integer overflow checks when doing sum, difference, multiplication and bit-shift

_Note that Syx can be compiled also without GMP. Even though you use an
image containing LargeIntegers, Syx will run without problems but won't understand that numbers._

See the [ChangeLog](http://syx.googlecode.com/svn/tags/syx-0.1.2/ChangeLog) for a complete list of things that changed.