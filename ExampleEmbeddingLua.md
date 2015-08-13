# Embedding a scripting language inside your C/C++ code #

## Disclaimer ##

**First of all, this is a remake of the article "[Embedding a scripting language inside your C/C++ code](http://www.debian-administration.org/articles/264)" of the Lua programming language for Smalltalk YX.**

## Introduction ##

**_These examples can be found in the source distribution of Syx in the `examples/embedding` directory._**

Smalltalk is a well known programming language thanks to its pure object-oriented programming phylosophy and its powerful features such as the graphical environment and reflection.
At all, many people don't know what are all the features that Smalltalk YX offers to developers. One of these is the easy embeddability into C/C++ applications.

There are many languages (such as Lua) with the main purpose to be used as scripting languages, but this is the first Smalltalk version you can use for both general and embedding purposes.

This article, briefly describes the main operations you can perform trough the C interface provided by Syx:
  * Initialise Syx
  * Call from C into a Syx script.
  * Call from Syx into your C application

## Initialising Smalltalk YX ##

The main thing to do before start scripting with Syx into your application, is initializing the entire interpreter engine:

```
#include <syx/syx.h>

int main (int argc, char *argv[])
{
  /* initialize Syx */
  syx_init (argc, argv, NULL);
  
  /* load the default image */
  syx_memory_load_image (NULL);
  
  /* cleanup Syx */
  syx_quit ();
  
  return 0;
}
```

Once you have saved this as init.c you can compile it by typeing:

```
$ gcc -o init init.c -Wall -lsyx
```

If you didn't get any errors, you should be able to execute the program and get no output from it:

```
$ ./init
$
```

The `syx_init()` function prepares the external environment of Syx such as the root path and the image path. `syx_memory_load_image()` loads the image at the given location: in this case we specified NULL which belongs to the default image path. Finally, `syx_quit()` frees all the memory used by Syx without saving the image.

## Calling from C into a Smalltalk script ##

Now it's time code more and know how C can interact with Syx. These two examples show the basic operations you can perform to reach the most common purposes for your application.

The first example will illustrate how to load a Smalltalk file, something like emulating the same behavior of `syx do-me.st` from the command line.

The second example pass some arguments to a Smalltalk method in a way much closer to C.

Copy these lines of code in a file named `do-me.st`:

```
'Start' printNl
1 to: 10 do: [ :i |
    i printNl ].
'End' printNl
```

Now create `do-me.c` file to load this file without scheduling the operation:

```
#include <syx/syx.h>

int main (int argc, char *argv[])
{
  /* initialize Syx */
  syx_init (argc, argv, NULL);
  
  /* load the default image */
  syx_memory_load_image (NULL);
  
  /* now file in our file in blocking mode, .i.e without scheduling the Process */
  syx_file_in_blocking ("do-me.st");

  /* cleanup Syx */
  syx_quit ();
  
  return 0;
}
```

Now compile the program as showed in the previous example and run it. The output will be the same as doing `syx do-me.st` from the command line:

```
$ gcc -o do-me do-me.c -lsyx -Wall
$ ./do-me 
Start
1
2
3
4
5
6
7
8
9
10
End
$
```

Let's create a class named `Sum` and a simple method `#with:and:` that returns the result of adding two numbers. This is the `add.st` file:

```
Object subclass: #Sum
       instanceVariableNames: ''
       classVariableNames: ''!

!Sum methodsFor: 'examples'!

with: firstNumber and: secondNumber
      ^firstNumber + secondNumber
! !
```

I hope you have some knowledge of the Smalltalk interchange format, this _cold_ syntax has been created to let the developer interchange Smalltalk code between different Smalltalk implementation and also to write code outside the Smalltalk environment.

Let's create a C interface called `add.c` and do the equivalent of `Sum new with: 3 and: 4`:

```
#include <syx/syx.h>

int main (int argc, char *argv[])
{
  SyxOop instance;
  SyxOop context;
  SyxOop process;
  SyxOop result;

  /* initialize Syx */
  syx_init (argc, argv, NULL);
  
  /* load the default image */
  syx_memory_load_image (NULL);
  
  /* now file in class and method declarations from our ST file */
  syx_file_in_blocking ("add.st");

  /* create a Sum instance */
  instance = syx_object_new (syx_globals_at ("Sum"));
  
  /* create a Process */
  process = syx_process_new ();

  /* create a MethodContext which sends the #with:and: message */
  context = syx_send_message (instance,                    // the receiver
			      "with:and:",                 // the selector
			      2,                           // the number of arguments
			      syx_small_integer_new (41),   // first argument
			      syx_small_integer_new (22));

  /* enter the context in the created process */
  syx_interp_enter_context (process, context);

  /* execute the process in blocking mode */
  syx_process_execute_blocking (process);

  /* fetch the last returned object (an instance variable of Process) */
  result = SYX_PROCESS_RETURNED_OBJECT (process);

  printf ("The result is %d\n", SYX_SMALL_INTEGER (result));

  /* cleanup Syx */
  syx_quit ();
  
  return 0;
}
```

_Smalltalk YX uses its own scheduler with its own threads (called processes). Each method or block being executed runs respectively into a MethodContext or BlockContext inside a Process. This is why we need that way of doing things, and the reason for which you can save the entire state of the interpreter in the image._

Compile `add.c` in the usual way and you see the result:

```
$ gcc -o add add.c -lsyx -Wall
$ ./add
The result of the sum is: 63
$
```

## Call from Syx into your C application ##

Smalltalk YX is also able to do the inverse thing as above very easily.
There're several methods to call C from Smalltalk:
  * Use standard primitives, which are available into the interpreter for doing basic operations
  * Use a plugin

The second is what we need to extend Syx with external libraries. You can either create a method with a special syntax to boost up the call or use the general purpose method `Smalltalk>>#plugin:call:withArguments`.
In this example, we'll use `Smalltalk>>#call:withArguments:` to interface directly into the main program.

The `average.st` file is the following:

```
| avgAndSum |
avgAndSum := Smalltalk cCall: 'average' withArguments: #(10 20 30 40 50).
('The average is ', avgAndSum first printString) printNl.
('The sum is ', avgAndSum second printString) printNl
```

And this is the `average.c` file containing the `average` function:

```
#include <syx/syx.h>

SYX_FUNC_PRIMITIVE(average)
{
  syx_int32 i;
  syx_int32 sum = 0;
  SyxOop oop;

  /* obtain the number of arguments that has been sent */
  syx_int32 args = es->message_arguments_count;

  /* create the array to hold both the average and the sum */
  SyxOop result = syx_array_new_size (2);

  for (i=0; i < args; i++)
    {
      /* get the nth argument */
      oop = es->message_arguments[i];
      /* assert the object is a SmallInteger */
      if (!SYX_IS_SMALL_INTEGER (oop))
	{
	  SYX_PRIM_FAIL;
	}

      /* get the integer value of the object and sum */
      sum += SYX_SMALL_INTEGER (oop);
    }
  
  /* put into the array the two results */
  SYX_OBJECT_DATA(result)[0] = syx_small_integer_new (sum / args);
  SYX_OBJECT_DATA(result)[1] = syx_small_integer_new (sum);
  
  SYX_PRIM_RETURN(result);
}

int main (int argc, char *argv[])
{
  /* initialize Syx */
  syx_init (argc, argv, NULL);
  
  /* load the default image */
  syx_memory_load_image (NULL);
  
  /* now file in our file in blocking mode, .i.e without scheduling the Process */
  syx_file_in_blocking ("average.st");

  /* cleanup Syx */
  syx_quit ();
  
  return 0;
}
```

`SYX_FUNC_PRIMITIVE` is a macro to create a primitive function for Syx. _We use a macro to mantain code compatibility for future versions_.
The `es` veriable is an argument of the `average` function, automatically defined by the macro.
This variable contains all the state of the interpreter, exactly it's the `SyxExecState` structure.
Notice how the interface is pretty simple and easy to understand.

Smalltalk doesn't support multiple return values, so we need to put both the average and the sum into an array. The macro `SYX_PRIM_FAIL` exits from the primitive and enters a Syx method that either raises an exception or perform operations similar to the primitive that fails to emulate the same behavior in Smalltalk itself.

Compile this with the **`-rdynamic`** option to allow the dynamic loader to lookup the `average` function (it's not needed on Windows):

```
$ gcc -o average average.c -lsyx -Wall -rdynamic
$ ./average
The average is 30
The sum is 150
$
```

This is basically what Syx currently offers when embedded, and will be surely improved in the future.