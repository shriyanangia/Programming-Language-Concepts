module Datum where


-- GHC
import Data.Maybe (listToMaybe)

-- ./prov
import Interface

--------------------------------------------------------------------------------
-- The MiniScheme Expression AST.
-- We refer to all MiniScheme Expressions as Datum.

data Datum =
  Num Int
  | Const Bool
  | Symbol String
  | Nil
  | Cons Datum Datum deriving (Show, Eq)

instance SchemeData Datum where
  symbol = Symbol
  number = Num . fromIntegral
  boolean = Const
  cons = Cons
  nil = Nil
  proper = foldr Cons Nil
  improper xs end = foldr Cons end xs

-- =============================================================================
-- Projective destructors ...
-- =============================================================================
-- These destructors 'project' out of the Datum type into Maybe a.

getNum :: Datum -> Maybe Int
getNum (Num n) = Just n
getNum _       = Nothing

getBool :: Datum -> Maybe Bool
getBool (Const b) = Just b
getBool _         = Nothing

getSymbol :: Datum -> Maybe String
getSymbol (Symbol s) = Just s
getSymbol _          = Nothing

-- =============================================================================
-- List destructors & helpers.
-- =============================================================================

-- The inverse of proper: Take some `datum` and flatten to a list. For example,
-- `(e1 e2 ... en Nil)` is turned into the list [e1, e2, ... , en] :: [Datum].
flatten :: Datum -> [Datum]  
flatten (Cons l r) = l : flatten r
flatten x          = [x]

-- Is this list of Datum proper? It is if it has `Nil` at the end.
-- N.B. The inverse of `isProper` is *NOT* `isImproper`:
-- `isProper` returns False if the list is improper OR
-- simply malformed.
-- Also note that we take as input [Datum] rather than Datum --
-- this is just for convenience.
isProper :: [Datum] -> Bool
isProper = (== Just Nil) . listToMaybe . reverse
