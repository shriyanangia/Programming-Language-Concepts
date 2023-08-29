module Tests where

import System.Exit
import System.Environment ( getArgs )
import Test.HUnit

import ChallengeTests
import Checker
import Parser
import Data.List (partition)
import Text.Read (readMaybe)

type TestCases = [(String, Maybe String)]

-- #############################################################################
-- Representing and evaluating simple data.
-- #############################################################################

--------------------------------------------------------------------------------
-- (1) constants
--------------------------------------------------------------------------------
constTests :: TestCases
constTests =
    [ ("1", Just "1")
    , ("-1", Just "-1")
    , ("#t", Just "#t")
    , ("#f", Just "#f")
    ]

-- #############################################################################
-- Evaluating Special forms
-- #############################################################################

--------------------------------------------------------------------------------
-- (5) if
--------------------------------------------------------------------------------
ifTests :: TestCases

ifTests =
    [ ("(if #t 1 2)", Just "1")
    , ("(if #f 1 2)", Just "2")
    , ("(if 1 2 3)", Just "2")
    , ("(if (if 1 2 3) 2 3)", Just "2")
    , ("(if)", Nothing)
    , ("(if 1)", Nothing)
    , ("(if (if 1) 2 3)", Nothing)
    , ("(if #t 1 (if))", Just "1")
    , ("(if #f (if) 1)", Just "1")
    ]

--------------------------------------------------------------------------------
-- (6) cond
--------------------------------------------------------------------------------

condTests :: TestCases
condTests =
    [ ("(cond [else 1])", Just "1")
    , ("(cond [#t 1] [else 2])", Just "1")
    , ("(cond [#f 1] [#t 2])", Just "2")
    , ("(cond [#t 1] [#f 2])", Just "1")
    , ("(cond [(cond [else #t]) 1] [else 2])", Just "1")
    , ("(cond [(cond [else #f]) 1] [else 2])", Just "2")
    , ("(cond [#t (cond [#f 1] [else 2])] [else 3])", Just "2")
    , ("(cond)", Nothing)
    , ("(cond [1 2 3])", Nothing)
    , ("(cond [else 1] [1 2 3])", Nothing)
    , ("(cond [#t 1] [#f (cond)])", Just "1")
    , ("(cond [#f (cond)] [#t 1])", Just "1")
    ]

--------------------------------------------------------------------------------
-- (7) and
--------------------------------------------------------------------------------

andTests :: TestCases
andTests =
    [ ("(and)", Just "#t")
    , ("(and #t)", Just "#t")
    , ("(and #f)", Just "#f")
    , ("(and #t #f)", Just "#f")
    , ("(and #f #t)", Just "#f")
    , ("(and 1 2)", Just "2")
    , ("(and 3 2 1)", Just "1")
    , ("(and (and 3 2) 1)", Just "1")
    , ("(and 1 (and 2 3))", Just "3")
    , ("(and #f (if))", Just "#f")
    , ("(and (if) #f)", Nothing)
    , ("(and 1 (if) 2)", Nothing)
    , ("(and 1 2 (if))", Nothing)
    ]

--------------------------------------------------------------------------------
-- (8) or.
--------------------------------------------------------------------------------

orTests :: TestCases
orTests =
    [ ("(or)", Just "#f")
    , ("(or #f #t)", Just "#t")
    , ("(or #t #f)", Just "#t")
    , ("(or 1 2 3)", Just "1")
    , ("(or #f 1 2)", Just "1")
    , ("(or #f #f #f)", Just "#f")
    , ("(or (or #t #f) #f)", Just "#t")
    , ("(or (or 1 2) #f)", Just "1")
    , ("(or 1 (or #t #f))", Just "1")
    , ("(or 1 (if))", Just "1")
    , ("(or (if) 1)", Nothing)
    , ("(or #f (if))", Nothing)
    ]

-- #############################################################################
-- Primitive functions
-- #############################################################################

--------------------------------------------------------------------------------
-- (9) eq?
--------------------------------------------------------------------------------

eqTests :: TestCases
eqTests =
    [ ("(eq? #t #t)", Just "#t")
    , ("(eq? #f #f)", Just "#t")
    , ("(eq? 0 #t)", Just "#f")
    , ("(eq? 0 #f)", Just "#f")
    , ("(eq? (cons #t #f) (cons #t #f))", Just "#t")
    , ("(eq? (cons #t #f) (cons #f #t))", Just "#f")
    , ("(eq? (cons 1 (cons 2 3)) (cons 1 (cons 3 2)))", Just "#f")
    , ("(eq? (cons 1 (cons 2 3)) (cons 1 (cons 2 3)))", Just "#t")
    , ("(eq? #t #t #t)", Nothing)
    , ("(eq? #t)", Nothing)
    , ("(eq? (eq? 1 1) #t)", Just "#t")
    , ("(eq? (eq? 1 1) (eq? 1 2))", Just "#f")
    , ("(eq?)", Nothing)
    ]

--------------------------------------------------------------------------------
-- (10) +
--------------------------------------------------------------------------------
plusTests :: TestCases
plusTests =
    [ ("(+)", Just "0")
    , ("(+ 1)", Just "1")
    , ("(+ -1)", Just "-1")
    , ("(+ 1 2 -1 -2)", Just "0")
    , ("(+ (+ 1 2) (+ -1 -2))", Just "0")
    , ("(+ 1 (+ 2) (+ 3))", Just "6")
    , ("(+ 1 #t)", Nothing)
    , ("(+ #t 1)", Nothing)
    , ("(+ 1 2 (+) (+ #f))", Nothing)
    ]

--------------------------------------------------------------------------------
-- (11) -
--------------------------------------------------------------------------------
minusTests :: TestCases
minusTests =
    [ ("(- 1)", Just "1")
    , ("(- -1)", Just "-1")
    , ("(- 0 1)", Just "-1")
    , ("(- 0 -1)", Just "1")
    , ("(- 3 2 1)", Just "0")
    , ("(- 3 -2 -1)", Just "6")
    , ("(- 3 (- 2))", Just "1")
    , ("(- 3 (- 0 2))", Just "5")
    , ("(-)", Nothing)
    , ("(- 1 (-))", Nothing)
    , ("(- 1 #t)", Nothing)
    , ("(- 1 (- 1 #t))", Nothing)
    ]

--------------------------------------------------------------------------------
-- (12) *
--------------------------------------------------------------------------------
timesTests :: TestCases
timesTests =
    [ ("(*)", Just "1")
    , ("(* 1)", Just "1")
    , ("(* 2 3)", Just "6")
    , ("(* 1 2 3)", Just "6")
    , ("(* 1 2 (* 3))", Just "6")
    , ("(* (*) 2 (* 3 4))", Just "24")
    , ("(* 1 #t)", Nothing)
    , ("(* #t 1)", Nothing)
    , ("(* (*) #t)", Nothing)
    , ("(* (*) (* #t))", Nothing)
    ]

--------------------------------------------------------------------------------
-- (13) =
--------------------------------------------------------------------------------
equalsTests :: TestCases
equalsTests =
    [ ("(= 1)", Just "#t")
    , ("(= 1 2)", Just "#f")
    , ("(= 1 1 2)", Just "#f")
    , ("(= 1 1 1)", Just "#t")
    , ("(= 1 (+ 0 1) (- 1 0))", Just "#t")
    , ("(=)", Nothing)
    , ("(= 1 #t)", Nothing)
    , ("(= #t)", Nothing)
    , ("(= #t #t)", Nothing)
    , ("(= 1 (= 1 1))", Nothing)
    ]

--------------------------------------------------------------------------------
-- (14) <, <=, >, and >=
--------------------------------------------------------------------------------
comparisonTests :: TestCases
comparisonTests =
    [ ("(< 1)", Just "#t")
    , ("(< 1 2)", Just "#t")
    , ("(< 1 2 3)", Just "#t")
    , ("(< 1 2 2)", Just "#f")
    , ("(< -1 1 3)", Just "#t")
    , ("(< 3 1 -1)", Just "#f")
    , ("(<)", Nothing)
    , ("(< 1 #t)", Nothing)
    , ("(< #f 1)", Nothing)
    , ("(< 1 (< 2 3))", Nothing)
    , ("(<= 1)", Just "#t")
    , ("(<= 1 2)", Just "#t")
    , ("(<= 1 2 3)", Just "#t")
    , ("(<= 1 2 2)", Just "#t")
    , ("(<= -1 1 3)", Just "#t")
    , ("(<= 3 1 -1)", Just "#f")
    , ("(<=)", Nothing)
    , ("(<= 1 #t)", Nothing)
    , ("(<= #f 1)", Nothing)
    , ("(<= 1 (<= 2 3))", Nothing)
    , ("(> 1)", Just "#t")
    , ("(> 1 2)", Just "#f")
    , ("(> 1 2 3)", Just "#f")
    , ("(> 1 2 2)", Just "#f")
    , ("(> -1 1 3)", Just "#f")
    , ("(> 3 1 -1)", Just "#t")
    , ("(> 3 1 1)", Just "#f")
    , ("(>)", Nothing)
    , ("(> 1 #t)", Nothing)
    , ("(> #f 1)", Nothing)
    , ("(> 1 (> 2 3))", Nothing)
    , ("(>= 1)", Just "#t")
    , ("(>= 1 2)", Just "#f")
    , ("(>= 1 2 3)", Just "#f")
    , ("(>= 1 2 2)", Just "#f")
    , ("(>= -1 1 3)", Just "#f")
    , ("(>= 3 1 -1)", Just "#t")
    , ("(>= 3 1 1)", Just "#t")
    , ("(>=)", Nothing)
    , ("(>= 1 #t)", Nothing)
    , ("(>= #f 1)", Nothing)
    , ("(>= 1 (>= 2 3))", Nothing)
    ]

--------------------------------------------------------------------------------
-- (15) cons
--
-- Note that our parser treats improper lists and nested cons's identically, so
-- "(1 . (2 . 3))" will be parsed as a correct answer to the third test case.
--------------------------------------------------------------------------------
consTests :: TestCases
consTests =
    [ ("(cons 1 2)", Just "(1 . 2)")
    , ("(cons (cons 1 2) 3)", Just "((1 . 2) . 3)")
    , ("(cons 1 (cons 2 3))", Just "(1 2 . 3)")
    , ("(cons #t (cons #t #t))", Just "(#t #t . #t)")
    , ("(cons 1 (cons #t (cons 3 4)))", Just "(1 #t 3 . 4)")
    , ("(cons (cons (-) 1) 2)", Nothing)
    , ("(cons (-) (cons 1 2))", Nothing)
    , ("(cons)", Nothing)
    , ("(cons 1)", Nothing)
    , ("(cons 1 2 3)", Nothing)
    ]

--------------------------------------------------------------------------------
-- (16) fst
--------------------------------------------------------------------------------
fstTests :: TestCases
fstTests =
    [ ("(fst (cons 1 2))", Just "1")
    , ("(fst (cons 2 1))", Just "2")
    , ("(fst (cons (cons 1 2) 3))", Just "(1 . 2)")
    , ("(fst (cons 1 (cons 2 3)))", Just "1")
    , ("(fst (cons #t (cons 2 3)))", Just "#t")
    , ("(fst (fst (cons (cons 1 2) 3)))", Just "1")
    , ("(fst 1)", Nothing)
    , ("(fst #t)", Nothing)
    , ("(fst (fst (cons 1 (cons 2 3))))", Nothing)
    , ("(fst (cons (if) 1))", Nothing)
    , ("(fst)", Nothing)
    , ("(fst 1 2)", Nothing)
    , ("(fst (cons 1 2) 2)", Nothing)
    , ("(fst 1 (cons 1 2))", Nothing)
    ]

--------------------------------------------------------------------------------
-- (17) snd
--------------------------------------------------------------------------------
sndTests :: TestCases
sndTests =
    [ ("(snd (cons 1 2))", Just "2")
    , ("(snd (cons 2 1))", Just "1")
    , ("(snd (cons (cons 1 2) 3))", Just "3")
    , ("(snd (cons 1 (cons 2 3)))", Just "(2 . 3)")
    , ("(snd (cons 1 (cons #t 3)))", Just "(#t . 3)")
    , ("(snd (fst (cons (cons 1 2) 3)))", Just "2")
    , ("(snd (snd (cons 1 (cons 2 3))))", Just "3")
    , ("(snd 1)", Nothing)
    , ("(snd #t)", Nothing)
    , ("(snd (snd (cons (cons 1 2) 3)))", Nothing)
    , ("(snd (cons 1 (cons 2 (if))))", Nothing)
    , ("(snd)", Nothing)
    , ("(snd 1 2)", Nothing)
    , ("(snd (cons 1 2) 2)", Nothing)
    , ("(snd 1 (cons 1 2))", Nothing)
    ]

--------------------------------------------------------------------------------
-- (18) list
--------------------------------------------------------------------------------

listFTests :: TestCases
listFTests =
    [ ("(list)", Just "()")
    , ("(list 1)", Just "(1)")
    , ("(list 1 2)", Just "(1 2)")
    , ("(list (list 1 2))", Just "((1 2))")
    , ("(list 1 (list 2 3))", Just "(1 (2 3))")
    , ("(list #t #f)", Just "(#t #f)")
    , ("(fst (list 1 2))", Just "1")
    , ("(snd (list 1 2))", Just "(2)")
    , ("(list 1 (if) 2)", Nothing)
    , ("(list 1 (list 2 (if)))", Nothing)
    ]

--------------------------------------------------------------------------------
-- (19) number?
--------------------------------------------------------------------------------
numberTests :: TestCases
numberTests =
    [ ("(number? 1)", Just "#t")
    , ("(number? #f)", Just "#f")
    , ("(number? (cons 1 2))", Just "#f")
    , ("(number? (fst (cons 1 2)))", Just "#t")
    , ("(number? (+ 1 2))", Just "#t")
    , ("(number? (number? 1))", Just "#f")
    , ("(number? (if))", Nothing)
    , ("(number?)", Nothing)
    , ("(number? 1 2)", Nothing)
    ]

--------------------------------------------------------------------------------
-- (20) boolean?
--------------------------------------------------------------------------------
boolQTests :: TestCases
boolQTests =
    [ ("(boolean? #t)", Just "#t")
    , ("(boolean? #f)", Just "#t")
    , ("(boolean? 1)", Just "#f")
    , ("(boolean? 0)", Just "#f")
    , ("(boolean? (cons 1 2))", Just "#f")
    , ("(boolean? (and 1 2))", Just "#f")
    , ("(boolean? (and 1 #f))", Just "#t")
    , ("(boolean? (boolean? 1))", Just "#t")
    , ("(boolean?)", Nothing)
    , ("(boolean? #t #t)", Nothing)
    , ("(boolean? (boolean?))", Nothing)
    ]

--------------------------------------------------------------------------------
-- (21) pair?
--------------------------------------------------------------------------------
pairTests :: TestCases
pairTests =
    [ ("(pair? 1)", Just "#f")
    , ("(pair? #t)", Just "#f")
    , ("(pair? (cons 1 2))", Just "#t")
    , ("(pair? (list 1 2))", Just "#t")
    , ("(pair? (fst (list 1 2)))", Just "#f")
    , ("(pair? (snd (list 1 2)))", Just "#t")
    , ("(pair? (snd (list 1)))", Just "#f")
    , ("(pair? 1 2)", Nothing)
    , ("(pair? (cons 1 2) 2)", Nothing)
    , ("(pair?)", Nothing)
    , ("(pair? (cons (pair? 1)))", Nothing)
    ]

--------------------------------------------------------------------------------
-- (21.5) nil?
--------------------------------------------------------------------------------

nilTests :: TestCases
nilTests =
    [ ("(nil? 1)", Just "#f")
    , ("(nil? #t)", Just "#f")
    , ("(nil? #f)", Just "#f")
    , ("(nil? (list 1))", Just "#f")
    , ("(nil? (fst (list 1)))", Just "#f")
    , ("(nil? (snd (list 1)))", Just "#t")
    , ("(nil?)", Nothing)
    , ("(nil? (fst (list 1)) (snd (list 1)))", Nothing)
    , ("(nil? (nil?))", Nothing)
    , ("(nil? (snd (list 1)) (fst (list 1)))", Nothing)
    ]

--------------------------------------------------------------------------------
-- (22) list?
--------------------------------------------------------------------------------
listQTests :: TestCases
listQTests =
    [ ("(list? 1)", Just "#f")
    , ("(list? #f)", Just "#f")
    , ("(list? (list 1 2))", Just "#t")
    , ("(list? (cons 1 (list 2 3)))", Just "#t")
    , ("(list? (cons (list 1 2) 3))", Just "#f")
    , ("(list? (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 (cons 7 (cons 8 (cons 9 (cons 10 (cons 11 (cons 12 (cons 13 (cons 14 (cons 15 (cons 16 (cons 17 (cons 18 (cons 19 (cons 20 (list 21))))))))))))))))))))))", Just "#t")
    , ("(list? (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 (cons 7 (cons 8 (cons 9 (cons 10 (cons 11 (cons 12 (cons 13 (cons 14 (cons 15 (cons 16 (cons 17 (cons 18 (cons 19 (cons 20 21)))))))))))))))))))))", Just "#f")
    ]

--------------------------------------------------------------------------------
-- #############################################################################
-- Small programs (Cumulative).
-- #############################################################################

--------------------------------------------------------------------------------

buildTests testCases =
    test [check i p r | (i, (p, r)) <- testCases]

p1Tests = concat
          [ constTests, ifTests, condTests, andTests, orTests, eqTests, plusTests,
            minusTests, timesTests, equalsTests, comparisonTests, consTests,
            fstTests, sndTests, listFTests, numberTests, boolQTests, pairTests,
            nilTests, listQTests ]

p1Start, p1End, c1Start, c1End :: Int
p1Start = 0
p1End   = length p1Tests
c1Start = p1End
c1End   = c1Start + length c1Tests

allTests = zip [0..] $ concat [p1Tests, c1Tests]

testSets =
    [ ("p1", [p1Start, p1End])
    , ("c1", [c1Start, c1End])]

main :: IO ()
main = do args <- getArgs
          let (makeJson, testIdxs)  = partition ("json" ==) args
              tests | null testIdxs = pickTests [p1Start, p1End]
                    | otherwise     = pickTests (ranges testIdxs)
          if not (null makeJson)
          then putStrLn (testsJson tests)
          else runTests tests

  where ranges [] = []
        ranges (s : ss)
            | Just idxs <- lookup s testSets = go idxs ++ ranges ss
            | Just i <- readMaybe s          = i : ranges ss
            | otherwise                      = error "Unknown ranges"
            where go []           = []
                  go (i : j : ks) = i : j : go ks

        pickTests (i : j : ks) = between i j ++ pickTests ks
        pickTests [i]          = [allTests !! i]
        pickTests []           = []

        between i j = take (j - i) (drop i allTests)

        runTests cases =
            do results <- runTestTT (buildTests cases)
               if errors results + failures results == 0
               then exitWith ExitSuccess
               else exitWith (ExitFailure 1)

        testsJson tests =
            unlines [ "{\"name\" : \"Test case [" ++ show n ++ "]: " ++ s ++ "\", \"setup\" : \"\", \"run\" : \"cabal run Test " ++ show n ++ "\", \"input\" : \"\", \"output\" : \"\", \"comparison\" : \"included\", \"timeout\" : 0.5, \"points\" : 1 },"
                    | (n, (s, _)) <- tests
                    ]
