module ChallengeTests (c1Tests) where

import System.Exit
import System.Environment ( getArgs )
import Test.HUnit

import Checker
import Parser

type TestCase = (String, Maybe String)
type TestCases = [TestCase]

--------------------------------------------------------------------------------
-- (1) Quote special form: Constants & basic constructions
--------------------------------------------------------------------------------

-- Can quote constants
bool'Tests :: TestCases
bool'Tests = [
  ("'#t", Just "#t"),
  ("'#f", Just "#f")
  ]

var'Tests :: TestCases
var'Tests = [
  ("'x", Just "x"),
  ("'abc", Just "abc"),
  ("'f1", Just "f1"),
  ("(quote f1)", Just "f1")
  ]

num'Tests :: TestCases
num'Tests = [
  ("'3", Just "3"),
  ("'123", Just "123"),
  ("'-1", Just "-1"),
  ("(quote -1)", Just "-1")
  ]

--------------------------------------------------------------------------------
--  Quote special form: Quoting primitive and special forms
--------------------------------------------------------------------------------

improperLists'Tests :: TestCases
improperLists'Tests = [
  ("'(d1 . (d2 . (d3 . d4)))", Just "(d1 . (d2 . (d3 . d4)))"),
  ("'(d1 d2 d3 . d4)", Just "(d1 d2 d3 . d4)"),
  ("(quote (x . y))", Just "(x . y)")
  ]

properLists'Tests :: TestCases
properLists'Tests = [
  ("'(1 2 3)", Just "(1 2 3)"),
  ("'(x y)", Just "(x y)"),
  ("'()", Just "()"),
  ("(quote (1 2 3))", Just "(1 2 3)")
  ]

-- The act of quoting primitives is (or should be) actually no different than
-- quoting a list, but it's worth checking these are *really* not executed.
prim'Tests :: TestCases
prim'Tests = [
  ("'(eq? 1 2)", Just "(eq? 1 2)"),
  ("'(+ 1 (+ 2 3))", Just "(+ 1 (+ 2 3))"),
  ("'(= 1 2 3)", Just "(= 1 2 3)"),
  ("(quote (number? (1 2 3)))", Just "(number? (1 2 3))")
  ]

specialForm'Tests :: TestCases
specialForm'Tests = [
  ("'(or 1 2)", Just "(or 1 2)"),
  ("(cond [(eq? 1 1) 1] '[else 2])", Just "1"),
  ("(cond ['(eq? 1 1) 3] '[else 2])", Just "3"),
  ("'(and 1 (and 2 3))", Just "(and 1 (and 2 3))"),
  ("(quote (cond [#f 1] [#t 2] [else 3]))", Just "(cond [#f 1] [#t 2] [else 3])")
  ]

--------------------------------------------------------------------------------
-- Quote special form: Nested quotes
--------------------------------------------------------------------------------

qqqqqqqqqquote n =
        concat $ replicate n "(quote " ++ ["x"] ++ replicate n ")"

nested'Tests :: TestCases
nested'Tests = [
  ("''x", Just "'x"),
  ("'''x", Just "''x"),
  ("'(1 '(2 3))", Just "(1 '(2 3))"),
  (qqqqqqqqqquote 10, Just (qqqqqqqqqquote 9))
  ]

--------------------------------------------------------------------------------
-- Eval primitive function:
--------------------------------------------------------------------------------

evalFailsTests = [
  ("(eval)", Nothing),
  ("(eval 1 2 3 4 5)", Nothing)
  ]

evalBoolTests = [
  ("(eval #t)", Just "#t"),
  ("(eval #f)", Just "#f")
  ]

evalNumTests = [
  ("(eval 0)", Just "0"),
  ("(eval 123)", Just "123"),
  ("(eval -1)", Just "-1")
  ]

-- We can't really express anything interesting without quotes.  MiniScheme will
-- eagerly evaluate its arguments, anyway -- so all we can test sans quotes is
-- that (eval t) works when t is a number of bool.
evalNoQuotingPrimTests =
  [
    ("(eval (fst (cons 1 2)))", Just "1"),
    ("(eval (snd (cons 1 2)))", Just "2"),
    ("(eval (snd (cons 1 2)))", Just "2"),
    -- expected failures
    ("(eval (1 . 2))", Nothing),
    ("(eval (cons + (cons 1 (cons 2 ()))))", Nothing)
  ]

evalNoQuotingSpecialFormTests =
  [
    ("(eval (and #t #t #t #f))", Just "#f"),
    ("(eval (or #f #f #f 1))", Just "1"),
    ("(eval (cond [(eq? 1 1) 1] '[else 2]))", Just "1"),
    ("(eval (cons 1 (and #f #f #f x)))", Nothing)
  ]

-- The rest of these tests presume some form of quoting implemented.
-- Unfortunately, I do not believe we can test the correctness of eval
-- without evaluating quoted terms.
evalWithQuotingPrimTests =
  [
    ("(eval (cons '+ (cons 1 (cons 2 '()))))", Just "3"),
    ("(eval '(+ 1 2))", Just "3"),
    ("(eval '(fst (cons 1 2)))", Just "1"),
    ("(eval '(snd (cons 1 2)))", Just "2"),
    ("(eval '(snd (cons 1 2)))", Just "2"),
    ("(eval (cons 'list (cons 1 (cons 2 '()))))", Just "(1 2)"),
    ("(eval '(pair? '(1 . 2)))", Just "#t"),
    ("(list (eval '(+ 1 2)) 3)", Just "(3 3)")
  ]

evalWithQuotingSpecialFormTests =
  [
    ("(eval '(and #t #t #t #f))", Just "#f"),
    ("(eval '(or #f #f #f 1))", Just "1"),
    ("(eval '(cond [(eq? 1 1) 1] '[else 2]))", Just "1"),
    ("(eval '(cons 1 (and #f #f #f x)))", Just "(1 . #f)")
  ]

evalWithQuotingNestedTests =
  [
    ("(eval " ++ qqqqqqqqqquote 10 ++ ")", Just $ qqqqqqqqqquote 8),
    ("(eval (eval (eval 10)))", Just "10"),
    ("(eval (eval (eval '(and #t #f))))", Just "#f"),
    ("(+ (eval '(+ 1 2)) (eval '(+ 3 4)))", Just "10"),
    ("(eval (cons (cond [#t '+] [else '-]) (cons 1 (cons 2 '()))))", Just "3"),
    ("(eval (cons (cond [#f '+] [else '-]) (cons 1 (cons 2 '()))))", Just "-1")
  ]

--------------------------------------------------------------------------------
-- Splice special form:
--------------------------------------------------------------------------------


spliceFailsTests = [
  ("$()", Nothing),
  ("(splice e)", Nothing),
  ("$e", Nothing),
  ("$(+ 1 2)", Nothing)
  ]

spliceBoolTests = [
  ("'$#t", Just "#t"),
  ("'$#f", Just "#f")
  ]

spliceNumTests = [
  ("'$1", Just "1"),
  ("'$-1", Just "-1"),
  ("(quote (splice 123))", Just "123")
  ]

spliceImproperListsTests = [
  ("'$'(1 2 . rest)", Just "(1 2 . rest)"),
  ("(quote $(fst '(x . y)))", Just "x"),
  ("(quote $(snd '(x . y)))", Just "y")
  ]

spliceProperListsTests = [
  ("'$'(x y)", Just "(x y)"),
  ("'$'(eq 1 2)", Just "(eq 1 2)"),
  ("'$'(1 2 3)", Just "(1 2 3)"),
  ("'(splice (quote (x y)))", Just "(x y)"),
  ("'$()", Nothing)
  ]

splicePrimTests = [
  ("'(eq? $(eq? 1 2) #t)", Just "(eq? #f #t)"),
  ("(quote (splice (+ 1 (+ 2 3))))", Just "6"),
  ("'(+ $(fst '(x . y)) $(snd '(y . z)))", Just "(+ x z)"),
  ("'(* $(number? 1) $(number? 2))", Just "(* #t #t)")
  ]

spliceSpecialFormTests = [
  ("(or '$(and #t #f) '$(and #t #t))", Just "#t"),
  ("(and '$(or #t #f) '$(or #t #t))", Just "#t"),
  ("(and 'poo '$(+ 1 2))", Just "3"),
  ("'(and 'poo 'bar $(fst '(x . y)))", Just "(and 'poo 'bar x)")
  ]

spliceNestedTests = [
  ("'(x '(2 $(+ 2 3)))", Just "(x (quote (2 5)))"),
  ("'(x '(2 $(+ 2 3) $$(+ 2 3)))", Nothing),
  ("'(x '(2 $(+ 2 3) $(+ 2 6)))", Just "(x '(2 5 8))")
  ]

--------------------------------------------------------------------------------
-- export
--------------------------------------------------------------------------------

c1Tests :: [TestCase]
c1Tests = concat [
  -- quote tests
  bool'Tests, var'Tests, num'Tests,
  improperLists'Tests, properLists'Tests,
  prim'Tests, specialForm'Tests, nested'Tests,

  -- splice tests
  spliceFailsTests, spliceBoolTests, spliceNumTests,
  spliceImproperListsTests, spliceProperListsTests,
  splicePrimTests, spliceSpecialFormTests, spliceNestedTests,

  -- eval tests
  evalFailsTests, evalBoolTests, evalNumTests,
  evalNoQuotingPrimTests, evalNoQuotingSpecialFormTests,
  evalWithQuotingPrimTests, evalWithQuotingSpecialFormTests, evalWithQuotingNestedTests
  ]
