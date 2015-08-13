![http://syx.googlecode.com/svn/wiki/images/syx-home.png](http://syx.googlecode.com/svn/wiki/images/syx-home.png)
# Smalltalk YX #

**Smalltalk YX** is an open source programming language. It's an implementation of the [Smalltalk-80](http://www.smalltalk.org) standard.


---

> # _<font color='orange'>Notice:</font> Syx has moved to a [new homepage](http://syx.berlios.de)._**#

---**

Syx is written in C and has the following purposes:

  * Readable code
  * Flexibility trough easy creation of plugins
  * Highly portable
  * Optimized
  * Modern
  * Embeddable in C applications
  * Easy to use, powerful and well-structured environment
  * Small

# Downloads for version 0.1.7 #

To download and install Syx please follow the [InstallationInstructions](InstallationInstructions.md).

**Looking for a Smalltalk YX appliance? [Click here](http://www.rpath.org/rbuilder/project/syx/)** _(thanks to Thilo Pfennig)_.

# Current status #

## Smalltalk YX 0.1.7 has been released ##

See ReleaseNotes\_0\_1\_7 for a complete list of news about this version.

**_Reached features till now_**:

  * Stable VM without memory leaks (or almost) until now
  * Running on Windows, Linux, Mac OSX and several embedded systems equally well
  * Image compatible with 32-bit and 64-bit systems, big and little endian machines
  * Good interaction from C to Smalltalk
  * Easy to use plugin system to call C from Smalltalk
  * Several functions to easily embed Syx in C/C++ applications
  * Immediate small integers and characters
  * Float and LargeInteger numbers
  * Small
  * A simple method caching
  * Object finalization
  * Support for infinite-precision numbers
  * Scripting from command line with user-defined startup sequence
  * MSVC compilation support
  * System signal handling
  * Image recovery on crash
  * Entirely written in ANSI C respecting strict ISO
  * Double building system (both GNU Autotools and SCons)
  * Simple interface to deal with C structs/unions
  * One stack per Process, Contexts are created on demand
  * Asyncronous system loops, like console and GTK+

For further informations on future directions, please refer to our RoadMap.

You can also take a look at the https://git.berlios.de/cgi-bin/gitweb.cgi?p=syx;a=blob;f=ChangeLog;h=6b9c39f26f20595e7e7f1b1a57999384c06d6400;hb=HEAD to stay always update about changes.

The Virtual Machine is almost complete. Now it's just a matter of adding environment features and cover everything to build up a powerful Smalltalk world.

# Contribute #

If you want to help or just give me suggestions or even talk with me about the project and other related things, please contact me at "lethalman88" at "gmail" dot "com" or subscribe to the [discussion mailing list](http://groups.google.com/group/syx-discuss)

Testers and coders are obviously accepted. If you want, we would invite you to follow the CodingStyle of Syx

For any bug report, feature request, etc., you can also add an entry to the [Issue tracker](http://code.google.com/p/syx/issues/entry).

**Thanks...**