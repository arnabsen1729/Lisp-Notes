# Lisp - very basic guide

## Setup

We can use SBCL which is a high performance Common Lisp compiler. To set up our environment follow these steps based on you environment:

**Ubuntu/Debian**
To install SBCL on either, just run:

```bash
$ sudo apt-get install sbcl
```

**Arch Linux**
Since SBCL is available from the official repositories, you can install it with:

```bash
$ sudo pacman -S sbcl
```

**OS X**
To install SBCL on OS X, just do:

```bash
$ brew install sbcl
```

Now, once installed close and open a new terminal session and type

```
$ sbcl
```

It will open the REPL for Lisp

![Repl for lisp](https://cdn.discordapp.com/attachments/840277591163666432/842002821111873596/unknown.png)

Now just type `(+ 1 2)` and press enter. It should display `3`.

But, working on this REPL would be inefficient so we would rather use out favourite text editor create lisp files ( extension is `*.lisp`) and run them.

So create a lisp file `test.lisp` and enter this line `(print "Hello World")`. Now in our terminal run this:

```bash
$ sbcl --script test.lisp
```

If you are VSCode user like me, install this extension to get Lisp syntax highlighting.

![Lisp Extension](https://cdn.discordapp.com/attachments/840277591163666432/842012850091458611/unknown.png)

This should display `Hello World` and now we are good to go.

> REPL stands for Read-Eval-Print-Loop, it is similar to python interpreter where it reads the input from the user, then evaluates it, prints the results and then again continues this in loop.

>From within the environment provided by the REPL, you can define and redefine program elements such as variables, functions, classes, and methods; evaluate any Lisp expression; load files containing Lisp source code or compiled code; compile whole files or individual functions; enter the debugger; step through code; and inspect the state of individual Lisp objects.

## Understanding (+ 1 2)

Anything in parentheses is a list, in this case a list of three elements, the symbol +, and the numbers 1 and 2. Lisp, in general, evaluates lists by treating the first element as the name of a function and the rest of the elements as expressions to be evaluated to yield the arguments to the function. In this case, the symbol + names a function that performs addition. 1 and 2 evaluate to themselves and are then passed to the addition function, which returns 3. The value 3 is passed to the printer, which prints it.

Hence we get 3 as output.

## Why so weird?

The first things anyone from C++, Java, Python bg will notice is that why are there **so many parentheses**, and why like whyyyyyyy? **prefix notation**.

![img](https://media.giphy.com/media/Kg2tFStNdUsOmxv2GC/giphy.gif)

> I didn't find the answer, if you do let me know

## Basic Syntax

Now in Lisp, whatever is deliminated by parantheses and has space separated values is called a **list**.

Then we have **forms**. A **form** is a list with a command function name at the beginning, like for example
`(+ 1 2)` is a form because it has `+` which is command/function name and `1` and `2` are just params to be passed and hence the output will be `3`.

We can nest our forms inside another form, like this
`(+ 1 (+ 2 3))`

>Basically everything is a list inside of Lisp

## Comments

In Lisp comments start with `;`.

Mutli-line comments in Lisp is given by `#||` at the beginning and `||#` at the end.

Example:

```lisp
(+ 1 (+ 2 3)) ; nested forms (single comment)

#||
Example of multi-line comments
||#

```
