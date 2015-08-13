# Create a binary package #

## GNU Build ##

First of all decide if the package must provide plugins and which plugins you want to be built.
Then specify the right paths where to install Syx components (Smalltalk files, the image, the plugins, the library and the _syx_ command).
Type `./configure --help` to see a list of options.

Before anything else, be sure the build is clean:

```
@/syx $ make clean
```

Now you can build Syx:

```
@/syx $ make
```

Now instead of doing make install as usual specify an alternative path. Take care the path must be an absolute path (e.g. /home/myuser/syx-bin:

```
@/syx $ make install DESTDIR=/home/myuser/syx-bin
```

## SCons ##

The behavior of Scons with Syx is not the expected behavior you would have using GNU Make or other packages. You will see **prefix** option will have a different behavior here, so take care of this document and read it deeply.

The first step, is to configure Syx and compile it. If you want Syx to be configured in /usr then append **prefix=/usr**:
```
@/syx $ scons prefix=/usr
```

After a successful compilation, now it's time to create the binary distribution:
```
@/syx $ scons bdist
```
This will create a directory named **syx-_version_** under the source directory containing binaries ready to be distributed.

The **scons bdist** command didn't install any file in **prefix** so the only thing you would clean is your copy of Syx.