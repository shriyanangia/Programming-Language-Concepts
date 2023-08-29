# Project 2 Tasks

Project 2 includes four tasks, described here.  For detail on the language features in each task, you should also consult the description of [MiniScheme](MiniScheme.md).

The goal of project 2 is to extend MiniScheme with support for local definitions and first-class functions.  While this does not entail implementing *as many* special forms and primitive functions as you implemented for project 1, it will still require you to make cross-cutting changes to your implementation:

* Your evaluation function will have to be adapted to support (local) variables.
* Your AST will have to be extended to include user-defined and primitive functions
* You will need to add new cases to your evaluation function to support user-defined functions.

These tasks are broken down in more detail in the remainder of this document.


# Variables

Your first task is to extend your evaluator to support user-defined variables.  Concretely, this means:

* You will need to extend your evaluation function to take a mapping from names to data&mdash;called *the environment*&mdash;as an argument.  For example, if your previous had an evaluation function:

  ```hs
  eval :: Datum -> Maybe Datum
  ```

  Then you could turn this this into an evaluation function

  ```hs
  eval :: Datum -> [(String, Datum)] -> Maybe Datum
  ```

  Of course, your code might end up differing from this example; you might want a more complex representation of the environment, you might choose to have the arguments in a different order, and so forth.

* You should evaluate symbols as variables in the environment.  If a variable is not in the environment, evaluation should fail.

* You should ensure that primitive functions behave as if they are symbols in the environment.  That is to say: `+`, `=`, or `eq?` should behave as if they are variables, each naming a function with the corresponding primitive behavior.  Of course, there are multiple ways you could achieve this.  One approach would be to extend your AST to include a representation of primitive functions, and then define an initial environment that maps each primitive function name to the corresponding representation in your AST.

  This may seem like a bait-and-switch: if primitive functions needed to be treated as variables, why were they not specified this way in Project 1?  However, not only should you be able to reuse your Project 1 code with little modification, you might actually find it requires *less* modification than it would if you had to adapt all of your primitive function implementations to the new type of your evaluation function.

It is difficult to provide external tests for this stage of the project, as we do not yet have any special forms that introduce user-defined variables.  However, as you know the type of your evaluation function, you should be able to provide environments that, for example, associate variable `x` to the number `1` and `y` to the number `2`; you should then be able to confirm that `(+ x y y)` evaluates to `5` and `(+ x y z)` fails.

Your pretty printer is free to represent primitive functions any way you choose; we will only ever test the results of calling primitive functions.

# Local Definition

Your second task is to add support for local definition, via the `let*` special form.  The `let*` special form allows the programmer to supply a list of new variable definitions; for example:
```scheme
(let* ([x 1] [y 2] [z #f]) (if z x y))
```
should evaluate to `2`.  See the [MiniScheme](MiniScheme.md) description for a full account of `let*`.

The bindings introduced in `let*` do *not* persist past the end of the `let*` expression.  So, the following should *not* evaluate:
```scheme
(list (let* ([x 1] [y 2]) (+ x y)) x)
```
The `x` inside the first argument to `list` is given meaning by the surrounding `let*`; however, the second occurrence of `x` is outside the `let*` and so is an undefined variable.

Each binding in `let*` can see existing variable definitions.  For example:
```scheme
(let* ([x 1]) (let* ([y (+ x x)]) (+ y y)))
```
should evaluate to `4`.  Moreover, each binding can see preceding definitions in the *same* `let*`.  For example:
```scheme
(let* ([x 1] [y (+ x x)]) (+ y y))
```
should also evaluate to `4`.

# Lambda

Your third task is to add support for first-class functions, realized by the `lambda` special form.

There are three sub-tasks

You should extend your AST to include function objects.  A function object consists (at least) of the parameter list and the body of the function.  *Your choices here will be different if you are doing the [challenge problems](Challenge.md).*    Your pretty printer may represent function objects however you want; we will only test the results of calling function objects.

You should extend your evaluation function to support the `lambda` special form, which produces function objects.  Be sure to check the parameter list at the time the function object is defined; for example, the following does not evaluate, even though the `lambda`-defined function is never used:
```scheme
(let* ([f (lambda 1 1)]) 1)
```

Finally, you should extend your evaluation function to support applying function objects.  You should *not* assume that the `lambda` special itself will appear as the first argument of the combination, as in:
```scheme
((lambda (x) (+ x 1)) 2)
```
The lambda might be bound to a variable:
```scheme
(let* ([f (lambda (x) (+ x 1))]) (f 2))
```
or even itself the result of a computation:
```scheme
(((lambda (y)
    (lambda (x) (+ x 1)))
  1)
 2)
```

# The `apply` function

Your fourth task is to add support for the `apply` primitive function.  The function call
```scheme
(apply f e1 e2 .. en)
```
applies `f` to arguments `e1 e2 .. en`, *where `en` is interpreted as a list of arguments, not as a single argument*.

For example:
```scheme
(apply + (list 1 2 3))
```
results in `6`.  Similarly,
```scheme
(apply + 1 2 (list 3 4))
```
results in `10`.

Remember that `apply` is a primitive function, not a special form.  For example, the following evaluates to `10`:
```scheme
(apply apply + (list 1 2 (list 3 4)))
```
