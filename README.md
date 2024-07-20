## Overview
A simple Scheme interpreter built by following [this](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours) tutorial. The aim of this project was to learn Haskell and for a quick introduction to writing interpreters.

## How to use it
Compile with `ghc scheme.hs` to create a binary of the interpreter. You can then choose run the interpreter in interactive mode or execute a script by passing it as an argument.

For example, interactive mode:
    
    $ ./scheme
    scheme> (+ 1 2 3)
    6
    scheme> (define (square x) (* x x))
    ...
    scheme> (square 2)
    4


### The library
To make scripting easier, the file `lib.scm` contains predefined functions to mimic the standard library. To use it, put `(load "lib.scm")` at the beginning of you script. It can also be used in the REPL:

    $ ./scheme
    scheme> (load "lib.scm")
    "Library loaded correctly"
    scheme> (map (curry + 1) (list 1 2 3))
    (2 3 4)
    scheme> (filter even? (make-range 0 10))
    (0 2 4 6 8 10)

The library is also an example of what can be done with such a basic intepreter.
