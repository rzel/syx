# Available platforms #

Syx has been tested on the following platforms:
  * Debian GNU/Linux (x64)
  * Gentoo Linux (x86 and x64)
  * Slackware Linux 12.0
  * Foresight Linux 1.4.1
  * Arch Linux 2007.08-2
  * SunOS 5.10
  * Solaris 11 (Sparc and x86-64)
  * FreeBSD 6.1
  * Linux From Scratch 64-bit
  * Windows 98, XP, Vista
  * Windows CE (PocketPC and SmartPhone)
  * Mac OSX (PPC and Intel)

# System requirements #

  * [GNU MP Bignum Library](http://gmplib.org/) (_recommended_)
  * [GTK+](http://www.gtk.org) toolkit (_optional plugin_)
  * [Readline](http://ftp.gnu.org/gnu/readline/) for the console (_optional plugin, not supported on Windows_)

**SEE BELOW FOR SOURCES INSTALLATION INSTRUCTIONS**

**SOME PACKAGES/ARCHIVES ARE NOT UPDATED YET TO VERSION 0.1.7**

# Windows XP #

To install Syx on your Windows XP system, you must [download the installer here](http://code.google.com/p/syx/downloads/detail?name=Syx-0.1.7.exe&can=2&q=).

Alternatively, if you are on a limited system, you can still [download the binaries archive here](http://code.google.com/p/syx/downloads/detail?name=syx-0.1.7-bin-win32.zip&can=2&q=).
Extract the archive then Syx is ready to be used. File types are not updated though to match .st and .sim files.

Once you downloaded it, double click on it and then follow the instructions.

# Windows CE (PocketPC and Smartphone) #

To install Syx on your handled system you must [download the binary archive here](http://code.google.com/p/syx/downloads/detail?name=syx-0.1.7-bin-wince.zip&can=2&q=).

Extract the archive; a new directory will be created named **syx-0.1.7-bin-wince**. Upload the contents of the directory (not the directory) into the root of your device.

To run it just click on the **syx** application in your handled device.

# Foresight Linux #

To install Syx on your linux distribution perform the following command in a root shell prompt:
```
# conary update syx
```

This will install the **syx** package or update it if already installed.

# Arch Linux #

To install Syx on your linux distribution perform the following command in a root shell prompt:
```
# pacman -U syx
```

This will install the **syx** package or update it if already installed.

# Gentoo Linux #

There's no Gentoo official ebuild at the moment, but you may use Layman to install it from the Gentoo Sunrise User Overlay.
If you don't have layman yet, you can emerge it with the following commands:
```
emerge -va layman
echo "source /usr/portage/local/layman/make.conf" >> /etc/make.conf
layman -f -a sunrise
```

Once this is done without troubles, we can go ahead installing Syx:
```
emerge syx
```

To upgrade to a newer version of Syx:
```
layman -s sunrise
emerge syx
```

# Other binary packages #

_**Syx works on many other platforms, but below you'll see only available downloads from this website.**_

The following are the Smalltalk YX 0.1.7 archives:

![http://syx.googlecode.com/svn/wiki/images/linux-logo.gif](http://syx.googlecode.com/svn/wiki/images/linux-logo.gif) [Linux x86-64](http://code.google.com/p/syx/downloads/detail?name=syx-0.1.7-bin-linux-x86_64.tar.gz&can=2&q=)

![http://syx.googlecode.com/svn/wiki/images/solaris-logo.gif](http://syx.googlecode.com/svn/wiki/images/solaris-logo.gif) [Solaris 11 (Sparc 64-bit)](http://code.google.com/p/syx/downloads/detail?name=syx-0.1.6-bin-solaris11-sparc.tar.gz&can=4&q=) (thanks to blufox)

![http://syx.googlecode.com/svn/wiki/images/solaris-logo.gif](http://syx.googlecode.com/svn/wiki/images/solaris-logo.gif) [Solaris 11 x86](http://code.google.com/p/syx/downloads/detail?name=syx-0.1.6-bin-solaris11-x86.tar.gz&can=4&q=) (thanks to blufox)

![http://syx.googlecode.com/svn/wiki/images/solaris-logo.gif](http://syx.googlecode.com/svn/wiki/images/solaris-logo.gif) [Solaris 11 x86-64](http://code.google.com/p/syx/downloads/detail?name=syx-0.1.5-bin-solaris11-x86-64.tar.gz&can=2&q=) (thanks to blufox)

![http://syx.googlecode.com/svn/wiki/images/freebsd-logo.gif](http://syx.googlecode.com/svn/wiki/images/freebsd-logo.gif) [FreeBSD 6.1 x86](http://code.google.com/p/syx/downloads/detail?name=syx-0.1.5-bin-FreeBSD.6.1-x86-32.tar.gz&can=2&q=) (thanks to blufox)

Once downloaded, please read the **README** or **README-BINARIES** file.

# Installing from Sources #

Obtain source archive here: http://syx.googlecode.com/files/syx-0.1.7.tar.gz
Find more archives in the [Downloads](http://code.google.com/p/syx/downloads/list) section.

### Unstable ###

If you want to hack on Syx or being updated with the latest changes, you can download the **[snapshot of the master branch here](http://repo.or.cz/w/syx.git?a=snapshot;h=master;sf=tgz)**.

Alternatively, you can also get a working copy of the [Git repository](http://code.google.com/p/syx/wiki/GitSource?tm=4):

```
git clone http://repo.or.cz/r/syx.git
```
or
```
git clone git://repo.or.cz/syx.git
```

## Building with GNU build (autotools) ##

To build Smalltalk YX with this building system you need a **make** program (for example [GNU make](http://www.gnu.org/software/make/)).

**[Then click here and follow the instructions](GNUBuild.md)**

## Building with SCons ##

In order to compile and install Smalltalk YX successfully you have to get [SCons](http://www.scons.org) installed on your system.

**[Then click here and follow the instructions](SConsBuild.md)**