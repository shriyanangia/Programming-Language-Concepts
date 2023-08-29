# Challenge Tasks

Challenge tasks are *cumulative* over the course of the project.  That means that:

1. You can complete challenge tasks at any time, for full credit.  For example, even if you did not attempt the challenge problems when doing project 1, you can still implement `quote`, `eval`, and `splice` during project 2, for credit.

2. You should assume that future challenge tasks may depend on having completed prior challenge tasks.

# Project 1 Challenge Tasks

There are three challenge tasks for project 1, weighted equally.

## The `quote` special form

The first task is to implement a special form `quote`.  The `quote` special form simply returns its argument, unevaluated, as a Scheme datum.  For example, the expression `(quote (1 2 3))` returns the proper combination `(1 . (2 . (3 . ())))`.  Similarly, the proper expression `(quote (+ 1 2))` returns the proper combination `(+ . (1 . (2 . ())))`, where the first element in the combination is the *unevaluated* symbol `+`.

This provides a good opportunity to check that your `eq?` function is implemented properly: the expression

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

# Problem 2 Challenge Tasks

The challenge task for project 2 is to implement static scoping for `lambda`.

Static and dynamic scoping are different ways to interpret the variable references in a function definition.  Suppose that we have a function term that uses a variable that's *not* a parameter:
```scheme
(lambda (x) (+ x y))
```
Variable `x` (as the argument to `+`) is a parameter.  However, there are two ways to think about `y`:

* We could interpret it in the context in which the function is *called*&mdash;that is to say, each invocation of this function might see a different value of `y` (or indeed, not have a value of `y` at all).  This is called *dynamic scoping*, and is what is implemented in the non-challenge version of the project.

* Alternatively, we could interpret it in the context in which the function is *defined*&mdash;that is to say, invocations of the function all see the same value of `y`.  This is called *static scoping*.

Static scoping is more common in functional programming languages.  In fact, it is essential to how we have learned functional programming in Haskell.  Consider the "multiple argument" function
```hs
f x y = x + y
```
We know that this is actually interpreted as *nested* lambdas:
```hs
f = \x -> \y -> x + y
```
But note that in the inner lambda, the `x` is a variable from the context!  We can express the same thing in MiniScheme:
```scheme
(lambda (x) (lambda (y) (+ x y)))
```
But with dynamic scoping, this definition will not work: the `x` in the inner lambda will be interpreted at the *use* of the latter function.

The challenge problem, then, is to implement static scoping.  This modifies three parts of project 2: the representation of function objects, the `lambda` special form, and the implementation of function application.

## Closures

For the variables in a function to be interpreted in its defining environment, each function object needs to retain the environment in which it was defined.  This combination of a function object and its defining environment is known as a *closure*.

Your first task in the challenge problem is to extend your AST with closures (instead of function objects).  Your representation of closures is up to you; at a minimum, it consists of the parameters, the body of the function, and the environment in which the function was defined.  Your printer may represent closures however you want; we will only test the result of calling closures.

## The `lambda` special form

Your second task is to adapt the `lambda` special form to closures.  The behavior of `lambda` must be extended to capture not just the arguments and body, but also the environment at the point of definition.

## Application

Your third task is to adapt application to closures.  Application differs from its behavior in the base project in that the parameter-argument bindings extend the *saved* environment in the closure, not the environment at the point of the function call.  The body of the function is then evaluated in that extended environment, giving the result of the function call.

After the function call, the environment should be unchanged.

## Testing

The challenge problem for project 2 is assessed by the overall number of passing challenge tests.
