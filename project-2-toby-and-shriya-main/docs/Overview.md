# Overview

This document contains a general overview of project 2.  Specific details are contained in the other Markdown files in this directory.

Some sections of these documents are specific to project 2.  For those sections that are shared with project 1, project 2-specific entries are marked **[P2]**.

You should read all the documentation carefully before beginning to work on the project.

# Project 2 Tasks

Project 2 continues the implementation of the MiniScheme language, described in [MiniScheme.md](MiniScheme.md).  The language we built in project 1 was functional in conception only: programs could not define new functions, and none of the special forms or primitive functions manipulated functions.  In project 2, you will add special forms for local definitions and first-class function definitions, and a primitive operation that supports general use of multi-argument functions.

You will have four primary tasks in this project:

1. Extend your evaluation function to support user-defined variables
2. Implement a special form `let*` for local definitions
3. Implement a special form `lambda` that builds functions
4. Implement a primitive function `apply` that applies multi-argument functions to lists of arguments.

Each of these tasks is described in more detail in [Tasks.md](Tasks.md).

# Directory Organization

The project directory contains several subdirectories.

* `docs`: Contains the descriptions of the project and the MiniScheme language.
* `prov`: Contains Haskell code provided as part of the project.  **You should not change any code within the `prov` directory.**
* **[P2]** `prov/Soln`: Contains sample solution code for project 1.  You may either refer to this code in writing your own solution, or import code from `Soln` directly in your solution.  In the latter case, you **must** update the `Project.cabal` file to list the modules from `Soln` that you are using. *The project 1 sample solution will be released after the project 1 due date.*
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

**[P2]** The `minis` executable now also functions as a simple REPL (read-evaluate-print loop) *for the MiniScheme language*, if given no file arguments.  For example:
```shell
$ cabal run minis
Up to date.
] (+ 1 2 3)
6
] ((lambda args (apply + args)) 1 2 3)
6
]
```
You can exit the REPL by entering a blank line at the `]` prompt.

These behaviors are defined in the `main` function in `src/Main.hs`.  We do *not* use this main function in testing, so you are free to add additional output as you find useful.  `Test` is the test runner target, as in the problem sets.

# Testing

Our tests for the project are essentially end-to-end tests: each test generates a source MiniScheme expression, sends it to your evaluator *and* printer functions (via the `run` function you write), and then compares your result against the expected result.

Note that you do *not* have to match the exact formatting of the expected result (for example: you don't have to match spaces, line breaks, or so forth).  You do, however, have to produce valid syntax; for example, if the expected result is the list "(1 2 3)", and your output omits the parentheses "1 2 3", the test will not be considered passing.

This means that you have to have *some* evaluation and *some* printing code to pass any of the tests.  It does not mean you have to have *all* of the evaluation and printing code to pass any of the tests.  I strongly encourage you to work iteratively: implement the minimal functionality needed to pass tests, rather than trying to implement the entire evaluator before testing.

Your evaluation function will be expected to return a `Nothing` value if evaluation fails (for example, if you try to subtract an empty list of numbers, or take the length of an integer).  We have included test cases where evaluation should fail; those test cases will not be considered passing if your evaluator crashes (for example, raises an incomplete pattern match exception) rather than returning `Nothing`.

The test runner target has new functionality: in addition to running single tests (`cabal run Test 14`), you can also run ranges of tests (`cabal run Test 14 18`).

# Challenge

The challenge problem for project 2 consists of implementing lexical scoping for `lambda`, and is described in [Challenge.md](Challenge.md).  This will require a *different approach* to some of the tasks in project 2; if you are considering doing the challenge problems for project 2, you should read [Challenge.md](Challenge.md) *before* beginning the project.

Challenge problems are *cumulative* through all three projects.  That means:

1. You should assume that future challenge problems may rely on past challenge problems.
2. You may complete the challenge for each project during *any subsequent project*, for full credit.
