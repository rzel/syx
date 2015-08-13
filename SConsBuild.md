# Compiling Syx #

After you got the sources, type the following command in the syx directory

```
scons
```

**NB:** _to know what you can enable/disable please run `scons -h`_:

**If you get any error or warning, please refer everything to me or create an issue in the tracker**

To clean objects and everything in the build directory:

```
scons -c
```

# Testing it #

Once the build process is over, you can run tests to check if syx works correctly on your system:

```
scons test
```

**If you get errors from any of the tests, please contact me**

To clean test objects and .passed files:

```
scons test -c
```

# Installing #

Syx will install the executable in `bindir`, the library and plugins in `libdir` and the data (the image, Smalltalk code) in `datadir`:

```
scons install
```

On Windows, it will install everything in C:\Syx by default. Do `scons -h` for a complete list of options.

# Uninstalling #

Hopefully you won't ;)

```
scons install -c
```