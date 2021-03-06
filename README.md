## Table of contents

- [Table of contents](#table-of-contents)
- [Setup](#setup)
- [Understanding (+ 1 2)](#understanding--1-2)
- [Why so weird?](#why-so-weird)
- [Basic Syntax](#basic-syntax)
- [Comments](#comments)
- [Numbers](#numbers)
- [Strings](#strings)
- [Format](#format)
- [Function](#function)
- [Variables](#variables)
- [Constants](#constants)
- [Arithmetic Operations](#arithmetic-operations)
- [Conditional Operations](#conditional-operations)

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

**~%**

Represents new line.

**~a**

The most general-purpose directive is ~A, which consumes one format argument of any type and outputs it in aesthetic (human-readable) form.

```lisp
(format t "The value is: ~a ~%" 10)
(format t "The value is: ~a ~%" "foo")
(format t "The value is: ~a ~%" (list 1 2 3))
```

will display:

```
The value is: 10
The value is: foo
The value is: (1 2 3)
```

**~s**

This is similar to `~a` but shows quotes around the string value.

```lisp
(format t "The value is: ~s ~%" 10)
(format t "The value is: ~s ~%" "foo")
(format t "The value is: ~s ~%" (list 1 2 3))
```

will display

```
The value is: 10
The value is: "foo"
The value is: (1 2 3)
```

**~d**
Five closely related directives format integer values: ~d, ~x, ~o, ~b, and ~r. The most frequently used is the ~d directive, which outputs integers in base 10.

If we put `:` like this `~:d` it will print commas to format the number.

```lisp
(format t "The value is: ~d ~%" 10000000)
(format t "The value is: ~:d ~%" 10000000)
```

will display

```
The value is: 10000000
The value is: 10,000,000
```

**~f**
Used for floating point directive

```lisp
(format t "The value of PI is: ~f ~%" pi)
(format t "The value of PI is: ~,4f ~%" pi)
(format t "The value of PI is: ~e ~%" pi)
```

will display

```
The value of PI is: 3.141592653589793
The value of PI is: 3.1416
The value of PI is: 3.141592653589793d+0
```

## Function
The most basic functionalities of Lisp are:
1. Functions
2. Variables
3. Macros

We are discussing the first one right now.

To define new functions we use the `defun` name. Usually function names contain only alphabetic characters and hyphens, but other characters are allowed and are used in certain naming conventions.

Basic function structure looks like this:

```lisp
(defun function-name (parameter*)
  "Optional documentation string."
  body-form*)
```

To call the function we write:

```lisp
(function-name argument*)
```

[Function examples](./function.lisp)

Look at this function:

```lisp
(defun hello-word() (format t "Hello World ~%"))
```

Name of the function is `hello-word`, argument list is empty hence doesn't need additional parameters to call it.

Remember, the value of the last expression inside the function is the value that it returns.

**Q. Write a function that takes two arguments and displays the sum also returns it**

Ans. In the attached code examples linked above.

## Variables

It is the next building block. There are two types of variables:
1. Lexical (Local)
2. Dynamic (Global)

As we all know variables are names places that holds value. Unlike, languages like C++, Java variables are not typed, i.e they can hold any type of values.

> Remember Lisp is a strongly typed language.

Each time a function is called, Lisp creates new bindings to hold the arguments passed by the function's caller. A binding is the runtime manifestation of a variable.

We can use the `let` to create variables (*JS vibes*). Let takes a form as the first argument, in the form each item (atom in Lisp term) is a initialisation list in itself where the first item is the symbol and the second is the object to be initialised with. It also has a body and the entire `let` returns the last evaluated expression of the body.

```lisp
(let ((x 10) (y 20) z)
  ...)
```
[Variable examples](./variables.lisp)

When the `let` form is evaluated, all the initial value forms are first evaluated. Then new bindings are created and initialized to the appropriate initial values before the body forms are executed. Within the body of the `let`, the variable names refer to the newly created bindings. After the `let`, the names refer to whatever, if anything, they referred to before the `let`.

The **scope** of function parameters and LET variables refers to the area of the program where the variable name can be used to refer to the variable's binding. It is delimited by the form that introduces the variable.

If inital value no provided it stores `NIL` by default.

But if we want to specify global variables (dynamic variables) we use `defvar`.

>Global variables are conventionally named with names that start and end with *

```lisp
(defvar *myvar* 0
  "this is documentation line, 0 is the initial value here"
)
```

If we want to update or mutate a variable we use the `setf`. `setf` takes two parameter first is the variable whose value we want to set and second is the new value.

Each time a function is called, Lisp creates new bindings to hold the arguments passed by the function's caller. Remember function parameter holds object referrences.Thus, you can assign a new value to a function parameter within the body of the function, and it will not affect the bindings created for another call to the same function. But, if the object passed to a function is mutable and you change it in the function, the changes will be visible to the caller since both the caller and the callee will be referencing the same object.

## Constants

We can even declare constant variables which we cannot mutate in future. For that we use `defconstant`

## Arithmetic Operations

The basic operators are +, -, *, /

To get the remainder we use the `rem` or `mod` command.

We also have some other commands whose functionality is evident from the names.
Eg.
`expt`, `sqrt`, `exp`, `log`, `floor`, `ceiling`, `max`, `min`, `oddp`, `evenp`

For checking equality, we use the `eq` or `=` command. Similarly, we have the `>` `<` commands as well.

[Arithmetic Examples](./arithmetic.lisp)

## Conditional Operations

The most basic conditional forms in Lisp is the if then, the structure of the form is like this:

```
(if condition then-form [else-form])
```

This forms returns the true value or the else value, if the else value is not provided it will simply return `nil`.

**Let's quickly cover how to read data**
There is also a `read` function which takes input from a stream we specify. So if we call the read function, it will wait for our prompt and when we press enter it will return whatever we entered.

**Q. WAP to ask the name and age and check if the person is above 18**

A. [Conditional Examples](./conditional.lisp)
