# Release notes #

### Released Syx 0.1.5 version ###

This version includes yet more features, compatibility with other build systems on different platforms, important bug fixes and a great performance boost, which will be continued in next releases.
A list of the major changes:
  * MSVC support has been improved
  * ANSI C and strict ISO compilation
  * External declarations for C++
  * The ST runtime compiler has been replaced with a C-side compiler
  * Class creation issues from ST has been fixed
  * Tracebacks and error reporting have been improved
  * Install desktop files on GNU/Linux platforms
  * Support for the GNU building system has been added as an alternative to SCons
  * Build under MSYS has been fixed
  * Block ensuring has been fixed
  * New examples have been added such as web code browser and a simple GTK+ application
  * Dictionary rehashing has been fixed
  * A better implentation of OrderedCollection has been enhanced
  * Internal profiling and gprof support at compile-time
  * Created one stack per process against one stack per context

This version breaks the API for creating new processes and contexts from C and ST too.
Thanks for anyone who contributed to Syx.

See the [ChangeLog](http://syx.googlecode.com/svn/tags/syx-0.1.5/ChangeLog) for a complete list of things that changed.