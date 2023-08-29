# Overview

This document contains a general overview of project 1.  Specific details are contained in the other Markdown files in this directory.

You should read all the documentation carefully before beginning to work on the project.

# Project 1 Tasks

In project 1, you will begin implementing a language "MiniScheme", based (loosely) on the Scheme programming language.  The MiniScheme language is described in [MiniScheme.md](MiniScheme.md).

You have three primary tasks:

1. Implement an abstract syntax tree for MiniScheme.
2. Implement an evaluation function for MiniScheme, including a set of primitive, or built-in operations.
3. Implement a printer for MiniScheme.

Each of these tasks is described in more detail in [Tasks.md](Tasks.md).

Finally, you will write a function `run` in the `Main.hs` module which ties together your evaluator and printer.

# Directory Organization

The project directory contains several subdirectories.

* `docs`: Contains the descriptions of the project and the MiniScheme language.
* `prov`: Contains Haskell code provided as part of the project.  **You should not change any code within the `prov` directory.**
* `src`: Contains source code that you write as part of the project.

Finally, there are the usual dot files in the root of the project folder.

You can put all your project code into `Main.hs`.  However, you may find it more useful to divide your project code among several modules&mdash;for example, an `AST` module for your syntax tree, a `Printer` module for your pretty printer, and so forth.

If you add additional modules to your project, you **must** update the `Project.cabal` file in the root of the project accordingly.  You should add each new module you create to the `other-modules` list in **both** the `minis` and `Test` targets.  Please see [the Cabal documentation](https://cabal.readthedocs.io/en/3.6/intro.html) for more details.

# Executables

`Project.cabal` defines two executable targets.  `minis` is an evaluator for MiniScheme programs: it takes a list of files, and interprets the contents of those files as MiniScheme expressions using your evaluation and printing functions.  For example, you could invoke:
```shell
$ cabal run minis examples/add.scm
0
3
6
6
10
$
```
This behavior is defined in the `main` function in `src/Main.hs`.  We do *not* use this main function in testing, so you are free to add additional output as you find useful.  `Test` is the test runner target, as in the problem sets.

# Testing

Our tests for the project are essentially end-to-end tests: each test generates a source MiniScheme expression, sends it to your evaluator *and* printer functions (via the `run` function you write), and then compares your result against the expected result.

Note that you do *not* have to match the exact formatting of the expected result (for example: you don't have to match spaces, line breaks, or so forth).  You do, however, have to produce valid syntax; for example, if the expected result is the list "(1 2 3)", and your output omits the parentheses "1 2 3", the test will not be considered passing.

This means that you have to have *some* evaluation and *some* printing code to pass any of the tests.  It does not mean you have to have *all* of the evaluation and printing code to pass any of the tests.  I strongly encourage you to work iteratively: implement the minimal functionality needed to pass tests, rather than trying to implement the entire evaluator before testing.

Your evaluation function will be expected to return a `Nothing` value if evaluation fails (for example, if you try to subtract an empty list of numbers, or take the length of an integer).  We have included test cases where evaluation should fail; those test cases will not be considered passing if your evaluator crashes (for example, raises an incomplete pattern match exception) rather than returning `Nothing`.

# Challenge

The challenge problems for project 1 consists of implementing several additional special forms, which draw on the ability to represent Minischeme programs as Minischeme data, and is described in [Challenge.md](Challenge.md).
