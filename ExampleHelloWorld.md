# Introduction #

Smalltalk YX is an interpreted programming language based on the Smalltalk-80 standard.
_"Interpreted programming language"_ means the code is not compiled in machine code instead each command is interpreted at run-time.

The interpreter frontend of Smalltalk YX is called **syx**. On Linux it's an ELF file, on Windows it's an executable file with the EXE extension, and so on other platforms.

The all you need is a Syx version working on your computer.

# Launch Syx #

To launch Syx on Linux just type syx as follows:
```
$ syx
```

On Windows, if you used the installer, you'll find the executable in Start->Programs->Smalltalk YX menu.


You'll see this once Syx has been launched:
```
>
```

This means Syx is ready and it's waiting for your input.

# Say Hello World #

Let's say something for the first time with Syx, type the following in the _syx_ interpreter:
```
'Hello world'!
```

Well, you can see now **Hello world** in output.

## Explanation ##

Strings in Smalltalk are represented using single quotes.
The exclamation mark at the end says to Syx to end the current block of statements.

Anyway, what you did was only creating a string, not printing it. It has been printed out because Syx automatically prints the latest evaluated object, which in this case was our input string `'Hello world'`.

To really print our string type the following command:
```
'Hello world' printNl!
```

You'll see **Hello world** printed two times. This because we print the `'Hello world'` string and finally Syx prints the latest evaluated object, which is `'Hello world'` as explained above.

# Standard Hello world #

The **printNl** method used above is a method introduced by Syx to print objects out the easy way.
By default on other Smalltalk, this is done by using the **Transcript** object, which represent an output buffer shown to the user.
We are working on a console, in fact our Transcript is just the stdout stream.
Try it:
```
Transcript showCr: 'Hello world'!
```

You'll see **Hello world** and **a TextCollector** printed below. What's that? Well, each object in Syx has a class. Here, the Transcript is an object created by the `TextCollector ` class. Since here the latest evaluated object is the Transcript, Syx will print out the class to which the object belongs to.

# Conclusion #

That's it :)