# Basic #

  * The C style is very close to the one adopted by Glib
  * Typical GNU indentation 2 spaces, default in Emacs
  * Use spaces instead of tabs
  * Use spaces between function names and parenthesis, not necessarily for macros

# Naming #

  * Names are lower case with underlines, except for structures.
  * Each structure has its own typedef.
  * Files are named as syx-COMPONENT.{c,h}
  * Use macros or inline functions when possible
  * Functions names have this form:
```
syx_COMPONENT_new
syx_COMPONENT_do_something
syx_COMPONENT_set_variable
syx_COMPONENT_get_variable
```

# Patches #

Once you create a patch, if you can, also add a [ChangeLog](http://syx.googlecode.com/svn/trunk/ChangeLog) entry with your name, email and the files you changed and why.

# Accessing objects #

  * Each object has its own data that can be accessed through the macro SYX\_OBJECT\_DATA(object), for example:
```
SyxOop var = SYX_OBJECT_DATA(object)[index];
SYX_OBJECT_DATA(object)[index] = var;
```
  * Variables in objects are accessed with the following macro:
```
SyxOop var = SYX_OBJECT_VARS(object)[index];
SYX_OBJECT_VARS(object)[index] = var;
```
  * Get data as syx\_symbol, syx\_string, byte array, float or large integer as shown below:
```
syx_symbol symbol = SYX_OBJECT_SYMBOL(oop);
syx_string string = SYX_OBJECT_STRING(oop);
syx_uint8 *byte_array = SYX_OBJECT_BYTE_ARRAY(oop);
syx_double float_num = SYX_OBJECT_FLOAT(oop);
mpz_t large_num = SYX_OBJECT_LARGE_INTEGER(oop);
```
  * Usually, there's a macro for each known variable of the most used objects:
```
SYX_CLASS_SUPERCLASS(class)
SYX_METHOD_SELECTOR(method)
SYX_PROCESS_STACK(process)
...
```
  * Don't use the **class** attribute to access the class of an object, call the following inlined functions instead:
```
syx_object_get_class (SyxObject *object)
syx_object_set_class (SyxObject *object)
```