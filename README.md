# Lisp - very basic guide

## Setup

We can use SBCL which is a high-performance Common Lisp compiler. To set up our environment follow these steps based on your environment:

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

But, working on this REPL would be inefficient so we would rather use our favorite text editor to create lisp files ( extension is `*.lisp`) and run them.

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

Anything in parentheses is a list, in this case, a list of three elements, the symbol +, and the numbers 1 and 2. Lisp, in general, evaluates lists by treating the first element as the name of a function and the rest of the elements as expressions to be evaluated to yield the arguments to the function. In this case, the symbol + names a function that performs addition. 1 and 2 evaluate to themselves and are then passed to the addition function, which returns 3. The value 3 is passed to the printer, which prints it.

Hence we get 3 as output.

## Why so weird?

The few things anyone from C++, Java, Python background will notice is that why are there **so many parentheses**, and why like whyyyyyyy? **prefix notation**.

![img](https://media.giphy.com/media/Kg2tFStNdUsOmxv2GC/giphy.gif)

> I didn't find the answer, if you do let me know

## Basic Syntax

Now in Lisp, whatever is deliminated by parentheses and has space-separated values is called a **list**.

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

## Numbers

Fairly simple, any sequence of digits- possibly prefaced with a sign (+ or -), containing a decimal point (.) or a solidus (/), or ending with an exponent marker is read as a number.

[Numbers examples](./numbers.lisp)

## Strings

They are enclosed in double-quotes. If we want to escape characters we use the `\` (backslash). But which chars can we escape? The answer is only 2.
1. Double quotes `"`
2. Backslash itself `\`
All other characters can be included in a string literal without escaping, regardless of their meaning outside a string.


[Strings examples](./strings.lisp)

If you execute this script using the command

```bash
$ sbcl --script strings.lisp
```

The output will be something like this:

```

"Hello"
"She replied \"Yes\""
"This \\ is backslash"
```

There are additional quotes in the strings. We can get rid of them if we use the `format` command instead of the `print`.

Tokens like the name of a command, variable, functions are represented by objects called *symbols*. One thing to keep in mind is to not using whitespace while naming these because, the elements of lists are separated by whitespace. Standard style, these days, is to write code in all lowercase and for separation use a dash `-`.

Two important constants that are defined this way are `T` and `NIL`, the canonical true and false values respectively.

## Format

By default `print` or `format` doesn't print a new line for that we use `~%` which is the equivalent of new line character.

[Format examples](./format.lisp)

The output will be:
```
Hello
She replied "Yes"
This \ is backslash
```
The `format` function takes two arguments
1. Destination for its output.
2. Control string that contains literal text and embedded directives.
3. There can be other arguments but they wouldn't be arguments to format function rather would interpolate values into the output. Known as *format arguments*.

| **First Arg** | **Means** |
|---|---|
| `t` | the output will be standard output i.e the console |
| `nil` | won't print rather return a string |

The second argument, the control string, is, in essence, a program in the FORMAT language. The directives that we pass to the control string may or may not need any arguments, like `~%` just emits a newline character and doesn't consume any arguments.

All directives start with `~` (tilde).
