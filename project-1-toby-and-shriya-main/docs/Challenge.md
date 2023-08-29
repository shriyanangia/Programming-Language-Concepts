# Project 1 Challenge Tasks

There are three challenge tasks for project 1, weighted equally.

## The `quote` special form

The first task is to implement a special form `quote`.  The `quote` special form simply returns its argument, unevaluated, as a Scheme datum.  For example, the expression `(quote (1 2 3))` returns the proper combination `(1 . (2 . (3 . ())))`.  Similarly, the proper expression `(quote (+ 1 2))` returns the proper combination `(+ . (1 . (2 . ())))`, where the first element in the combination is the *unevaluated* symbol `+`.

This provide a good opportunity to check that your `eq?` function is implemented properly: the expression

```scheme
(eq? (fst (quote (+ 1 2))) (quote +))
```

should evaluate to `#t`.

The provided parser supports two syntactic shortcuts for writing quoted expressions.  First, `'e` is equivalent to `(quote e)`.  So, the previous expression could have been written:

```scheme
(eq? (fst '(+ 1 2)) '+)
```

Second, a quoted list can be written in braces:

```scheme
(eq? (fst {+ 1 2}) '+)
```

You do not have to do anything extra to support this syntax.

## The `eval` primitive function

The second challenge task is to implement a primitive `eval`, which evaluates its argument as if it were a Scheme expression.  Note that evaluate is *not* a special form: in the expression `(eval e)`, first `e` is evaluated to find the argument to `eval`; second, the result of evaluating `e` is evaluated as if it were a Scheme expression.

Here is an example:

```scheme
(eval (cons '+ (cons 1 (cons 2 '()))))
```

The argument evaluates to the proper combination `(+ 1 2)`, (where `+` is so far an uninterpreted symbol).  Then, `eval` evaluates that resulting combination as a special form, producing 3.

## The `splice` special form

The final challenge task is to implement a special form `splice`.

This special form is unusual in that it is only meaningful *inside* a `quote` special form; `splice` outside of a `quote` fails to evaluate.

Inside a `quote`, the expression `(splice e)` is *not* included in the result of the `quote`; instead, the expression `e` is evaluated and its result is included in the `quote`.  For example, the term:

```scheme
{+ 1 (+ 1 2) 3}
```

evaluates to the proper combination `(+ 1 (+ 1 2) 3)`, where the `+`s are both uninterpreted symbols.  On the other hand, the term

```scheme
{+ 1 (splice (+ 1 2)) 3}
```

evaluates to the proper combination `(+ 1 3 3)`, where the spliced expression has been evaluated.

The provided parser supports a syntactic shortcut for writing splices: `$e` is equivalent to `(splice e)`.  So the above example could have been written `{+ 1 $(+ 1 2) 3}`.  You do not have to do anything extra to support this syntax.
