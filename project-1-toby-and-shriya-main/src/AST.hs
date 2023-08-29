module AST where

import Interface 


data Datum
    = Number Integer
    | Boolean Bool
    | Symbol String
    | Cons Datum Datum -- (d1 . d2)
    | Nil
    deriving (Show, Eq)

instance SchemeData Datum where
    symbol = Symbol
    number = Number
    boolean = Boolean
    cons = Cons
    nil = Nil

    proper [] = Nil
    proper (d:ds) = Cons d (proper ds)

    improper [] d = d
    improper (d':ds) d = Cons d' (improper ds d)

-- if the input is a proper list, then the result is "Just" a Haskell list
-- with each of the arguments
asProperList :: Datum -> Maybe [Datum]
asProperList (Cons x1 x2) =
    case asProperList x2 of
        Nothing -> Nothing
        Just x2' -> Just (x1 : x2')
asProperList Nil          = Just []
asProperList _            = Nothing

--takes a list of data and creates nested pairs
--does the same thing as the proper function
properList :: [Datum] -> Datum
properList ([]) = Nil
properList (d: ds) = Cons d (proper ds)

--printerHelper :: 
