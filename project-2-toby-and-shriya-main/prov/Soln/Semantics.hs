module Semantics (eval, eval_maybe) where

-- Haskell
import Data.Maybe (fromJust, listToMaybe)

-- ./prov
import Text.Parsec (runParser)
import Parser (expr, terminal)

-- ./src
import Datum ( Datum(..), getNum, flatten, isProper )
import Interface (SchemeData (proper))
import Printer (printDatums, printDatum)

import Prelude hiding (fail, const)
  
-- =============================================================================
-- Errors & Error handling
-- =============================================================================

-- The Result type has two constructors:
-- `Good` tells us an evaluation went well. It stores the evaluated `Datum`.
-- `Bad` tells us an evaluation failed. It stores an `Error` message.
data Error =  Error String
data Result = Good Datum | Bad Error

instance Show Error where
  show (Error s) = "Error: " ++ show s

instance Show Result where
  show (Bad e) = show e
  show (Good d) = show d

-- Result is more informative than `Maybe`, as we get an `Error` string rather
-- than just `Nothing`. This makes debugging easier. However, `Main.hs` expects
-- run to have type `Datum -> Maybe String`. To tie these tubes, we "forget" the
-- `Error` and turn it into `Maybe Datum`.
forgetError :: Result -> Maybe Datum
forgetError (Good r) = Just r
forgetError _        = Nothing  

--------------------------------------------------------------------------------
-- smart constructors, which take the regular old values (Int, String, Bool)
-- and build `Result`s rather than `Datum`. These are purely for convenience.

const :: Bool -> Result
const    = Good . Const

num :: Int -> Result
num      = Good . Num

cons :: Datum -> Datum -> Result
cons x y = Good (Cons x y)

true, false, nil :: Result
true     = const True
false    = const False
nil      = Good Nil

symbol :: String -> Result
symbol   = Good . Symbol

--------------------------------------------------------------------------------
-- Error constructors & error helpers

-- `fail` builds an `Error` result.
fail, notImplemented :: String -> Result
fail             = Bad . Error
notImplemented s = fail ("Feature not implemented: " ++ s)

bad :: Result
bad = fail "Something has gone wrong and I'm not sure why."

argsBorked, invalidArg :: String -> [Datum] -> Result
argsBorked s xs =
  fail ("Either not enough, or, too many arguments given to "
        ++ s ++ ". Arguments Given: " ++ show xs
        ++ ". Arguments Printy Printed: " ++ printDatums xs)
invalidArg s xs = 
  fail ("Invalid argument(s) supplied to "
       ++ s ++ ": " ++ show xs
       ++ ". Arguments Printy Printed: " ++ printDatums xs)

--------------------------------------------------------------------------------
-- Misc. Helpers

-- Pairwise application of `f`. For example,
--   pairwise (<) [1, 2, 3]
-- is
--   (1 <= 2) && (2 <= 3)
pairwise :: (a -> a -> Bool) -> [a] -> Bool
pairwise f xs = and (zipWith f xs (tail xs))

-- =============================================================================
-- Num util
-- =============================================================================

-- Scoop out the numbers in a Result.
getNumberR :: Result -> Maybe Int
getNumberR (Good n) = getNum n
getNumberR _        = Nothing

-- Turn a list of result numbers (n1 n2 ... n_m) into a list of numbers.
-- Will turn anything not a number into Nothing.
numbersInR :: [Result] -> Maybe [Int]
numbersInR = sequence . fmap getNumberR

-- Evaluate a list of expressions [e1, e2, ..., en] and scoop out any numbers.
getNumbers :: [Datum] -> Maybe [Int]
getNumbers = numbersInR . map eval . init

-- =============================================================================
-- Bool util
-- =============================================================================

-- Everything except #f is "truthy" in MiniScheme, meaning it behaves as "true"
-- in conditional statements.
truthy :: Datum -> Bool
truthy (Const False) = False
truthy _             = True

-- Truthy lifted to the Result type.
truthyR :: Result -> Result 
truthyR (Good d) = const (truthy d)
truthyR e        = e

-- =============================================================================
-- Evaluating special forms
-- =============================================================================

specialForms :: [String]
specialForms = ["if", "cond", "and", "or"]

isSpecialForm :: String -> Bool
isSpecialForm = (`elem` specialForms)

specialFormHandler :: String -> [Datum] -> Result

--------------------------------------------------------------------------------
-- if & cond conditionals

specialFormHandler "if" [d1, d2, d3, Nil] =
  case truthyR (eval d1) of
           Good (Const True)  -> eval d2
           Good (Const False) -> eval d3
           Bad e              -> Bad e
specialFormHandler "if" args = argsBorked "if" args

specialFormHandler "cond" xs =
    case cases xs of
      Right cs -> handleCases cs
      Left e   -> Bad e
  where
    handleCases :: [(Datum, Datum)] -> Result
    handleCases []                         = bad
    handleCases ((Symbol "else", e2) : xs) = eval e2
    handleCases ((e1, e2) : xs)            =
      case truthyR (eval e1) of 
       Good (Const True)  -> eval e2
       Good (Const False) -> handleCases xs
       Bad e              -> Bad e

    cases :: [Datum] -> Either Error [(Datum, Datum)]
    cases = sequence . init . fmap toCase
    
    toCase :: Datum -> Either Error (Datum, Datum)
    toCase (Cons x (Cons y Nil)) = Right (x, y)
    toCase xs                    = Left (Error ("bad conditional branch: " ++ show xs))


--------------------------------------------------------------------------------
-- "and" and "or" booleans

specialFormHandler "and" []    = true
specialFormHandler "and" [Nil] = true
specialFormHandler "and" xs    = conjunct xs
  where
    conjunct :: [Datum] -> Result
    conjunct [x, Nil] = eval x
    conjunct [x]      = eval x
    conjunct (x : xs) =
      case eval x of
        Good (Const False) -> false
        Good _             -> conjunct xs
        Bad e              -> Bad e

specialFormHandler "or" []    = false
specialFormHandler "or" [Nil] = false
specialFormHandler "or" xs    = disjunct xs
  where
    disjunct :: [Datum] -> Result
    disjunct [x, Nil] = eval x
    disjunct [x]      = eval x
    disjunct (x : xs) =
      case eval x of
        Good (Const False) -> disjunct xs
        x                  -> x

specialFormHandler s xs = argsBorked s xs    
-- =============================================================================
-- Evaluating primitives
-- =============================================================================

primitives :: [String]
primitives = [
  "eq?", "+", "*", "-", "=", "<", "<=", ">", ">=",
    "cons", "fst", "snd", "list", "number?",
    "boolean?", "pair?", "nil?", "list?"
  ]
isPrim :: String -> Bool
isPrim = (`elem` primitives)

primHandler :: String -> [Datum] -> Result

--------------------------------------------------------------------------------
-- Arithmetic operators (+, -, *)

primHandler "+" [] = argsBorked "+" []
primHandler "+" xs =
    case getNumbers xs of
      Just ns -> num (sum ns)
      Nothing -> bad

primHandler "*" [] = argsBorked "*" []
primHandler "*" xs =
  case getNumbers xs of
      Just ns -> num (product ns)
      Nothing -> bad

primHandler "-" [] = argsBorked "-" []
primHandler "-" xs =
    case getNumbers xs of
      Just (i : is) -> num (sum (i : map negate is))
      Just []       -> argsBorked "-" xs
      Nothing       -> argsBorked "-" xs

--------------------------------------------------------------------------------
-- Integer comparison operators

primHandler "=" [] = argsBorked "=" []
primHandler "=" xs =
    case getNumbers xs of
      Just [] -> bad
      Nothing -> bad
      Just ns -> const (pairwise (==) ns)
    
primHandler "<" [] = argsBorked "<" []
primHandler "<" xs =
    case getNumbers xs of
      Just [] -> bad
      Nothing -> bad
      Just ns -> const (pairwise (<) ns)  

primHandler "<=" [] = argsBorked "<=" []
primHandler "<=" xs =
    case getNumbers xs of
      Just [] -> bad
      Nothing -> bad
      Just ns -> const (pairwise (<=) ns)    

primHandler ">" [] = argsBorked ">" []
primHandler ">" xs =
    case getNumbers xs of
      Just [] -> bad
      Nothing -> bad
      Just ns -> const (pairwise (>) ns)      

primHandler ">=" xs =
    case getNumbers xs of
      Just [] -> bad
      Nothing -> bad
      Just ns -> const (pairwise (>=) ns)

--------------------------------------------------------------------------------
-- Operations on pairs & lists

primHandler "cons" [e1, e2, Nil] =
  case (eval e1, eval e2) of
    (Good v1, Good v2) -> cons v1 v2
    (Bad e1, Bad e2)   -> fail ((show e1) ++ " " ++ (show e2))
    _                  -> argsBorked "cons" [e1, e2, Nil]
primHandler "cons" args = argsBorked "cons" args

primHandler "fst" [x, Nil] =
  case eval x of
    Good (Cons v1 v2) -> Good v1
    Good _            -> argsBorked "fst" [x, Nil]
    Bad e             -> Bad e
primHandler "fst" args =  argsBorked "fst" args

primHandler "snd" [x, Nil] =
  case eval x of
    Good (Cons v1 v2) -> Good v2
    Good _            -> argsBorked "snd" [x, Nil]  
    Bad e             -> Bad e
primHandler "snd" args =  argsBorked "snd" args

primHandler "list" []    = Good Nil
primHandler "list" [Nil] = nil
primHandler "list" xs    =
  case sequence (map (forgetError . eval) xs) of
    Just ys -> Good (proper (init ys))
    Nothing -> bad

--------------------------------------------------------------------------------
-- Question operators

primHandler "eq?" [x, y, Nil] =
  case (eval x, eval y) of
    (Good x, Good y)   -> const (x == y)
    (Bad e, _) -> Bad e
    (_, Bad e) -> Bad e
primHandler "eq?" (x : y : z : xs) =
  argsBorked "eq?" (x : y : z : xs)
primHandler "eq?" args = argsBorked "eq?" args

primHandler "number?" [x, Nil] =
  case eval x of
    Good (Num _) -> true
    Good _       -> false
    Bad e        -> Bad e
primHandler "number?" args = argsBorked "number?" args

primHandler "boolean?" [x, Nil] =
  case eval x of
    Good (Const _) -> true
    Good _         -> false
    Bad e          -> Bad e
primHandler "boolean?" args = argsBorked "boolean?" args

primHandler "pair?" [x, Nil] =
  case eval x of
  (Good (Cons a b)) -> true
  (Good _) -> false
  (Bad e) -> Bad e
primHandler "pair?" args = argsBorked "pair?" args

primHandler "nil?" [x, Nil] =
  case eval x of
    Good Nil -> true
    Good _   -> false
    Bad e    -> Bad e
primHandler "nil?" args = argsBorked "nil?" args

primHandler "list?" (x : _) =
  case eval x of
    Good rest@(Cons a b) -> (Good (Const (isProper (flatten rest))))
    Good _               -> false
primHandler "list?" args =  argsBorked "list?" args

primHandler s xs = argsBorked s xs

-- =============================================================================
-- Evaluation
-- =============================================================================

eval :: Datum -> Result
eval (Num n)            = Good (Num n)
eval (Const b)          = Good (Const b)
eval Nil                = Good Nil
eval (Cons (Symbol s) rest) 
  | not (isProper flat) = argsBorked s flat
  | isPrim s            = primHandler s flat
  | isSpecialForm s     = specialFormHandler s flat
  | otherwise           = notImplemented s
  where
    flat                = flatten rest
eval (Symbol s)         = notImplemented s
eval d                  = argsBorked "eval" (flatten d)
  
eval_maybe :: Datum -> Maybe Datum
eval_maybe = forgetError . eval

--------------------------------------------------------------------------------
-- Debugging

parse :: String -> Datum
parse s =
    case runParser (terminal (expr True)) () "" s of
        Left err -> error (show err)
        Right d -> d

test :: String -> Result
test = eval . parse

--------------------------------------------------------------------------------
-- Example Usage

-- `parse` will show you the `Datum` a MiniScheme expression parses into.
-- >>> parse "(eq? 1 2)"
-- Cons (Symbol "eq?") (Cons (Num 1) (Cons (Num 2) Nil))

-- `eval` will evaluate a `Datum`.
-- >>> eval (Cons (Symbol "eq?") (Cons (Num 1) (Cons (Num 2) Nil)))
-- Const False

-- Of course, you can compose these two to get `test`.
-- `test` parses a string and then evaluates it.
-- >>> test "(if (eq? 1 2) #t #f)"
-- Const False

-- Because we are using the `Result` type instead of `Maybe`,
-- we can add in our own error messages. Here is what happens
-- when we try to evaluate a symbol.

-- >>> parse "x"
-- Symbol "x"
-- >>> test "x"
-- Error: "Feature not implemented: Variable assignment."
 
-- Here is what happens when we give too many arguments
-- to `if`

-- >>> test "(if 1 2 3 4 5)"
-- Error: "Either not enough, or, too many arguments given to if. Arguments Given: [Num 1,Num 2,Num 3,Num 4,Num 5,Nil]. Arguments Printy Printed: 1\n2\n3\n4\n5\n()"
