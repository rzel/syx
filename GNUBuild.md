# Compiling Syx #

After you got the sources, type the following command in the syx directory

```
./configure
```

**NB:** _to know what you can enable/disable please run `./configure --help`_:

**If you get any error or warning, please refer everything to me or create an issue in the tracker**

To clean objects and everything in the build directory:

```
make clean
```

# Testing it #

Once the build process is over, you can run tests to check if Syx works correctly on your system:

```
make check
```

**If you get errors from any of the tests, please contact me**

# Installing #

Syx will install the executable in `bindir`, the library and plugins in `libdir` and the data (the image, Smalltalk code) in `datadir`:

```
make install
```

On Windows, it is recommended to be into a Cygwin or MSYS environment.

# Uninstalling #

Hopefully you won't ;)

```
make uninstall
```