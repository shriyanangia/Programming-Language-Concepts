# MiniScheme

MiniScheme is an eagerly-evaluated functional programming language.  While MiniScheme is, like Haskell, based on the lambda-calculus, it differs from Haskell in several important ways:

* MiniScheme is *eagerly evaluated*: function arguments are evaluated before function calls, instead of delaying evaluation until argumenst are needed.
* MiniScheme is *untyped*: the same MiniScheme data may be given multiple interpretations depending on the context, and your MiniScheme evaluator will not be able to assume that arguments are of the right form.
* MiniScheme supports *multiple-argument functions*: unlike Haskell, where multi-argument functions are simulated with single-argument functions, MiniScheme has multiple argument functions.  In fact, many MiniScheme functions can take variable numbers of arguments.
* MiniScheme is *syntactically simple*: unlike Haskell, MiniScheme has a very simple syntax.

This document describes the MiniScheme language.  You should refer to it (as well as the examples and test cases) to direct your implementation of MiniScheme.

# Data

The core data structure of MiniScheme is the *datum*.  We will use data to represent both MiniScheme values and programs.  (In fact, one of the key features of languages like Scheme is that programs can be represented as data.)

Data comes in five kinds:

* Numbers `1`, `-217`, `42` are data.
* Booleans `#t`, `#f` are data.  In any use of Booleans, MiniScheme will treat all non-`#f` as truth.
* Symbols `x`, `+`, `f1` are data.  Symbols consist of any sequence of letters, numbers, and punctuation that does not start with a number or one of the characters `#`, `'`, `.`, `$`.
* If `d` and `e` are data, then the pair `(d . e)` is data.
* The "nil" value `()` is data.

Note that MiniScheme treats `()` and `[]` as equivalent.  That is, you could also choose to write the nested pairs `((1 . 2) . (3 . 4))` as `[(1 . 2) . (3 . 4)]`, `([1 . 2] . [3 . 4])` or `[[1 . 2] . [3 . 4]]`.  MiniScheme treats all of these identically.

There are two additional categorizations of data.  *Proper lists* are defined by:

* `()` is a proper list
* `(d . e)` is a proper list if `e` is a proper list.

The proper list `(d1 . (d2 . (d3 . ())))` can also be written `(d1 d2 d3)`.  (These representations are *equivalent*.)   Proper lists in MiniScheme play the role of lists in Haskell; they are also central to representing MiniScheme programs.

*Improper* lists are lists that do not end with `()`.  An improper list `(d1 . (d2 . (d3 . d4)))` can also be written `(d1 d2 d3 . d4)`.  (Note that, technically, any pair is an improper list, and the syntax for a two-element improper list aligns exactly with that of a pair.  Nevertheless, improper lists will play a role in defining MiniScheme programs later in the project.)

# Evaluation

Evaluation of MiniScheme programs is described as transformation on MiniScheme data&mdash;that is, MiniScheme programs and values are *both* represented using data.

Evaluation is defined by the following rules.

1. Numbers `-1`, `42` evaluate to themselves.

2. Boolean constants `#t`, `#f` evaluate to themselves.

3. Symbols `+`, `x` are treated as variables.  However, in this stage of the project we do not yet have any user-defined functions or values, so symbols will *only* evaluate if they name primitive functions.  (Primitive functions are defined later in this document.)

4. Proper lists `(d1 d2 ... dn)` evaluate depending on `d1`.  In general, there are three cases.

    - `d1` may be a symbol naming a *special form*&mdash;that is to say, a language feature of MiniScheme&mdash;such as `if`, `or,` `define`, or `lambda`.  In this case, the evaluation of `(d1 d2 ... dn)` is specific to that special form; rules for special forms are given in the next section.

    - `d1` may be an expression evaluating to a user-defined function.  At this point, we do not have user-defined functions, so we do not need to implement this case.

    - `d1` may be a symbol identifying a primitive function.  In this case, the remaining arguments `d2`, `d3`, ... are evaluated (following the same rules), and the primitive function is called with the resulting values as arguments.  Primitive functions are described following special forms.

5. Attempting to evaluate other forms of data is an error.

For example, suppose we were evaluating the datum `(+ 1 (+ 2 3))`.

- We would begin by observing the symbol `+`; this is the name of a primitive function.

- We would evaluate the arguments.  `1` is a number and so evaluates to itself.  The second argument is a combination, so we proceed (recursively):

  - We begin by observing symbol `+`; this is the name of a built-in function.

  - We would evaluate the arguments.  `2` and `3` are numbers and so each evaluates to itself.

  - We would then invoke the primitive `+` function with arguments `2` and `3`, getting result `5`.

- We would then invoke the primitive `+` function with arguments `1` and `5`, getting result `6`.

# Special Forms

Special forms are the "language features" of MiniScheme.  Because MiniScheme is eagerly evaluated, many features that are implemented as functions in Haskell are special forms in Scheme.  This section enumerates the special forms you should implement in your evaluator.

## Conditionals

MiniScheme has two forms of conditionals.

The simpler form is `if`.  The expression `(if d1 d2 d3)` is evaluated by first evaluating `d1`.  If `d1` evaluates to `#f`, then the result of evaluating the expression is the result of evaluating `d3`.  Otherwise, it is the result of evaluating `d2`.  For example, `(if #t 1 2)`, `(if (eq? 1 1) 1 2)` and `(if 0 1 2)` all evaluate to `1`.  On the other hand, `(if #f 1 2)` and `(if (eq? 1 2) 1 2)` evaluate to `2`.

The more complex form is `cond`.  The `cond` can be given any number (more than 1) of *branches*, each of which is the of the form `(e1 e2)` for two data `e1` and `e2`.  For example, the following are syntactically valid instances of `cond`:

```scheme
(cond [(eq? 1 1) 1] [else 2])
(cond [(eq? 1 2) (+ 1 1)] [(eq? 1 1) (- 1 2)] [#t 3])
(cond [#f 1] [#t 2] [else 3])
```

A `cond` special form is evaluated by checking the branches in order.  In a branch `(e1 e2)`, if `e1` evaluates to anything other than `#f`, the result of the `cond` is the result of evaluating `e2`.  Otherwise evaluation continues to the next branch.

If the symbol `else` appears as expression `e1`, then the result of the `cond` is `e2`.

If no `e1` evaluates to a non-`#f` value, or is the symbol `else`, evaluation should fail.

## Booleans

MiniScheme has two conditions for manipulating Booleans.

The special form `(and e1 e2 ... en)` computes the conjunction of `e1` ... `en`.  With no arguments, `(and)` evaluates to `#t.`  Otherwise, arguments to `and` are evaluated left-to-right.  If any argument evaluates to `#f`, the `and` evaluates to `#f` **without evaluating any further arguments**.  Otherwise, the `and` evaluates to the result of the final argument.  For example, `(and #f 1 2)` and `(and 1 2 #f)` both evaluate to `#f`; `(and 1 2 3)` evaluates to `3`.

The special form `(or e1 e2 ... en)` computes the disjunction of `e1` ... `en`.  With no arguments, `(or)` evaluates to `#f`.  Otherwise, the arguments to `or` are evaluated left-to-right.  If any argument evaluates to a non-`#f` value, the `or ` evaluates to that value **without evaluating any further arguments**.  Otherwise, the `or` evaluates to `#f`.  For example, `(or 1 2 3)` evaluates to `1`; `(or 3 2 1)` evaluates to `3`; and, `(or (eq? 1 2) (eq? 2 1))` evaluates to `#f`.

These are special forms rather than primitive functions because they need not evaluate all their arguments.

# Primitive functions

In addition to the special forms, MiniScheme also has a number of *primitive* (or built-in) functions.  Unlike special forms, these functions obey the normal evaluation rules: all of their arguments are evaluated before the function is applied.  However, they provide primitive functionality that otherwise could not be expressed with MiniScheme programs.  Each intrinsic function is defined for a specific number of arguments; calling it with a different number of arguments causes evaluation to fail.

* `(eq? e1 e2)`: if `e1` and `e2` evaluate to identical values, then this function returns `#t`.  Otherwise, it returns `#f`. With other than two arguments, evaluation fails.

* `(+ e1 e2 .. en)`: with no arguments, `(+)` returns 0.  If all its arguments evaluate to numeric values, `+` returns their sum; otherwise, evaluation fails.

* `(- e1 e2 .. en)`: with no arguments, evaluating `(-)` fails.  If all its arguments evaluate to numeric values, `-`, computes the difference `e1` - `e2` - `e3` - .. - `en`; otherwise, evaluation fails.

* `(* e1 e2 .. en)`: with no arguments, `(*)` returns 1.  If all its arguments evaluate to numeric values, `*` returns their product; otherwise, evaluation fails.

* `(= e1 e2 .. en)`: with no arguments, evaluating `(=)` fails.  If all its arguments evaluate to numeric values, then `(= e1 e2 .. en)` returns `#t` if their values are equal, and `#f` if their values are not; if any arguments evaluate to non-numeric values, evaluation fails.   The primitive functions `<`, `<=`, `>`, `>=` are defined similarly.  For example, `(<)` fails, while `(< e1 e2 .. en)` returns #t if each argument evaluates to a number less than the next argument, `#f` if one argument evaluates to a number greater than or equal to the next argument, and fails to evaluate if any argument evaluates to anything other than a number.

* `(cons e1 e2)`: returns the pair `(v1 . v2)`, where `v1` is the evaluation of `e1` and `v2` is the evaluation of `e2`.

* `(fst e)`: if `e` evaluates to a pair `(v1 . v2)`, returns `v1`.  Otherwise, evaluation fails.

* `(snd e)`: if `e` evaluates to a pair `(v1 . v2)`, returns `v2`.  Otherwise, evaluation fails.

* `(list e1 e2 .. en)`: evaluates to the proper collection `(e1 . (e2 . (... . (en . ())))`.  Without arguments, evaluates to the nil value.

* `(number? e)`: if `e` evaluates to a numeric value, returns `#t`; otherwise, returns `#f`.

* `(boolean? e)`: if `e` evalutes to a Boolean value, return `#t`; otherwise, returns `#f`.

* `(pair? e)`: if `e` evaluates to a pair value, returns `#t`; otherwise, returns `#f`.

* `(nil? e)`: if `e` evaluates to the nil value, returns `#t`; otherwise, returns `#f`.

* `(list? e)`: if `e` evaluates to a proper list, returns `#t`; otherwise, returns `#f`.


