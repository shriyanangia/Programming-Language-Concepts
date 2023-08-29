# Project 1 Tasks

Project 1 includes three tasks, described here.  For detail on each task, you should also consult the description of [MiniScheme](MiniScheme.md).

# Task 1: MiniScheme AST

Your first task is to define an *abstract syntax tree* for MiniScheme&mdash;that is, a data type that represents MiniScheme expressions.  This data type will serve as both the input and output of your evaluation function, as well as the input to your printing functions.

The Scheme standard calls Scheme's AST "data", and similarly we have called the AST type `Datum` (in `src/Main.hs`).  You do not have to keep this name, but if you do change it, you should change the type signature for `run` accordingly.  You also do not have to leave the AST definition in `Main.hs`; if you move it to another file, make sure to update `Project.cabal` to include that file in the `other-modules` for both the `minis` and `Test` targets.

You can define your AST as you choose.  However, we strongly recommend that you closely follow the specification of Scheme data given in [MiniScheme.md](MiniScheme.md).  In particular, note that proper and improper lists are *specific cases of* pairs, not an alternative to pairs.  If you represent them separately from pairs in your AST, you are likely to make later parts of the project more complicated.  (You may, of course, find it useful to write functions that map between pairs and more Haskell-friendly representations of proper and improper lists to use in writing your evaluator.)

You must also make your `Datum` type an instance of the `SchemeData` class, defined in `prov/Interface.hs`.  The `SchemeData` class provides a uniform interface to your `Datum` type, allowing our parser (`prov/Parser.hs`) to generate values in your `Datum` type.  It also provides the basis for how we will test your code.  As in the interface you defined in Problem Set 4, many of the functions in `SchemeData` will correspond directly (or almost directly) to constructors in your `Datum` type.  Again, if you choose to move your `Datum` type to a different module, you may wish to move its `SchemeData` instance to that module as well.

# Task 2: MiniScheme Evaluator

Your second task is to define an *evaluation function* for MiniScheme.  Your evaluator must implement the evaluation model and all the special forms and primitive functions described in the [MiniScheme description](MiniScheme.md).

Your evaluation function must not crash (for example, throw incomplete pattern match exceptions) in cases where evaluation is undefined.  To capture such cases, you will likely want your evaluation function to have a type like `Datum -> Maybe Datum`.

If you want to capture additional information about evaluation failures, you can use a type like `Datum -> Either String Datum`.  In this case, you should *not* change the type signature of `run`.  However, you are free to change the definition of `main` in `src/Main.hs` to print out the additional information you generate.

You may choose to put your evaluator code in a separate module from `Main.hs`; if so, be sure to update `Project.cabal` to include this module in `other-modules` for both the `minis` and `Test` targets.

# Task 3: MiniScheme Printer

Your third task is to define a *printer* for MiniScheme.  Your printer *must* follow the syntax given in the [MiniScheme description](MiniScheme.md): for example, the false constant *must* be printed as `#f`, and a list of `1`, `2`, and `3` can be printed as  `(1 2 3)` or `[1 2 3]`, but not as `(1, 2, 3)`, `[1; 2; 3]`, `List [Number 1, Number 2, Number 3]`, or so forth.

Where there are multiple representations of the same data, your printer can use *any* of those representations.  For example, the list of `1`, `2`, and `3` can be printed as `(1 2 3)`, `(1 . (2 3))`, `(1 . (2 . (3)))`, or `(1 . (2 . (3 . ())))`.

You may introduce spacing and line breaks as you choose&mdash;that is to say, your printer may be as "pretty" as you want.  However, Scheme does consider some amount of space necessary to separate tokens (that is, words) of the language: `(1.(2.(3.())))` is *not* a valid representation of the list of `1`, `2`, and `3`.

You may use the pretty-printing library developed in class while writing your printer.  You can import it as `Bird.Printer`.

You may choose to put your printing code in a separate module from `Main.hs`; if so, be sure to update `Project.cabal` to include this module in `other-modules` for both the `minis` and `Test` targets.

Finally, you should update the `run` function in `Main.hs` to call your evaluation function and, should it succeed, your printing function.
