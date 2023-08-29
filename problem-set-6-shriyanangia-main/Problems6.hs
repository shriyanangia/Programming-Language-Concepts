module Problems6 where

import Data.List (sortOn)
{-------------------------------------------------------------------------------

CS:3820 Fall 2022 Problem Set 6
================================

This problem set moves from Boolean to arithmetic formulae, but now restricted
to a single variable.  Our formulae consist of:

  - The unique variable (for historical reasons, let's call it x)
  - Integer constants
  - Products and sums of formulae
  - Formulae raised to positive integer exponents.

Note the restrictions on exponentiation: we're interested in formulae like x^2
and (x+1)^3, but *not* 2^x.

We define a data type to represent such formulae.

-------------------------------------------------------------------------------}

data Formula = X | C Int | Formula :+: Formula | Formula :*: Formula | Formula :^: Int
  deriving (Eq, Show)

infixl 6 :+:
infixl 7 :*:
infixr 8 :^:

{-------------------------------------------------------------------------------

Part 1: Distribution
---------------------

The first part of this week's problem is to write a function "distribute" that
applies the distributive laws of multiplication over addition and exponentiation
over both multiplication and addition:

   e * (f + g) = e * f + e * g
   (e + f) * g = e * g + f * g

   (e :^: n) :^: m = e :^: m * n

   (e * f) ^ n = e ^ n * f ^ n

   (e + f) ^ 0 = 1
   (e + f) ^ n = (e + f) * (e + f) ^ (n - 1)

In case you are wondering: yes, this is very similar to the second part of
problem set 5.

It is not required, but you may also find it helpful to implement constant
folding: for example, reducing (C 1 :+: C 2) to C 3, or (C 2 :^: 2) :^: 3 to C
64

This part is assessed as four problems:

  1. multiplication over addition
  2. exponentiation over addition
  3. exponentiation over multiplication and exponentiation

dnf :: Formula -> Formula
dnf ((f :\/: g) :/\: h) = dnf (f :/\: h) :\/: dnf (g :/\: h)
dnf (f :/\: (g :\/: h)) = dnf (f :/\: g) :\/: dnf (f :/\: h)
dnf (f :\/: g)          = dnf f :\/: dnf g
dnf (f :/\: g)          
    | isDisjunction f' || isDisjunction g' = dnf (f' :/\: g')
    | otherwise              = f' :/\: g'
    where 
        f' = dnf f 
        g' = dnf g

dnf x = x

-------------------------------------------------------------------------------}
isSum :: Formula -> Bool

isSum (f :+: g) = True 
isSum f = False

distribute :: Formula -> Formula

distribute (f :*: (g :+: h)) = distribute (f :*: g) :+: distribute (f :*: h)
distribute ((f :+: g) :*: h) = distribute (f :*: h) :+: distribute (g :*: h)

distribute (f :^: 0) = C 1
distribute ((f :+: g) :^: 1) = (f :+: g)
distribute ((f :+: g) :^: h) = distribute (expand h)
  where
    expand :: Int -> Formula
    expand 1 = f :+: g
    expand n = (f :+: g) :*: expand (n - 1)
  --where
  --  f' = distribute f
  --  g' = distribute g


distribute ((f :*: g) :^: h) = distribute (f :^: h) :*: distribute (g :^: h)
distribute ((f :^: g) :^: h) =  distribute(f :^: (g*h))

distribute (f :^: g) = distribute f :^: g

distribute (f :*: g)          
    | isSum f' || isSum g' = distribute (f' :*: g')
    | otherwise              = f' :*: g'
    where 
        f' = distribute f 
        g' = distribute g


distribute (f :+: g) = distribute f :+: distribute g
distribute (f) = f
-- In case it helps, here is a function to test whether or not a formula has
-- been fully distributed.  As with the previous problem set, you should *not*
-- need to call distributed in your implementation of distribute.
distributed :: Formula -> Bool
distributed = go 0 where
    go _ X         = True
    go _ (C _)     = True
    go n (e :+: f) = n < 1 && go 0 e && go 0 f
    go n (e :*: f) = n < 2 && go 1 e && go 1 f
    go n (e :^: _) = n < 2 && go 2 e

{-------------------------------------------------------------------------------

Part 2: Polynomials
-------------------

Of course, there's a more compact (and more useful) way to represent arithmetic
formulae on single variables: as polynomials

    a₀ + a₁x + a₂x² + a₃x³ + ...

For the second part of the problem you should write a function "poly" which,
given an *arbitrary* arithmetic formulae, returns a list of the coefficients aᵢ
in the corresponding polynomial.  For example, if your input formula was

    (X :+: C 2) :^: 3

then the corresponding polynomial is 8 + 12x + 6x² + x³, and so your poly
function should return the list [8,12,6,1].

You'll start by writing a useful worker function "coeff" which takes a single
summand---that is, an expression that does *not* contain any uses of :+:---and
returns a tuple (a, n) such that the original expression is equivalent to a*x^n.
For example, "coeff (X :*: C 2 :*: C 3 :*: X :^: 2)" should return the pair (6,
3).  You may assume any other invariants you established in your "distribute"
function.

This function is assessed as 3 points.

-------------------------------------------------------------------------------}

coeff :: Formula -> (Int, Int)
coeff f = go f (1,0)
  where
    go :: Formula -> (Int, Int) -> (Int, Int) 
    go X (c,e) = (c, e+1)
    go (X :^: e')(c,e) = (c, e+e')
    go (f1 :*: f2)(c,e) = 
      let (c1,e1) = go f1 (c,e)
      in go f2 (c1, e1) 
    go (C c')(c,e) = (c*c', e) 
    go (C n :^: e')(c,e) = (c, e+e') 

{-------------------------------------------------------------------------------

Finally, you'll write the "poly" function itself.  "poly" should make no
assumptions about its input; the previous functions you've written are likely to
be helpful.

This function is assessed as 3 points (Tests 8-10).

-------------------------------------------------------------------------------}
summand :: Formula  -> [Formula]
summand (a :*: b) = [a :*: b]
summand (a :+: b) = summand a ++ summand b
summand a = [a]

poly :: Formula -> [Int]
poly f = [fst (coeff f)]

