module AST where

import Interface 


data Datum
    = Number Integer
    | Boolean Bool
    | Symbol String
    | Cons Datum Datum -- (d1 . d2)
    | Nil
    -- | FunctionObject Datum [String] (Maybe String) --old
    -- ex: ((lambda (x . y) (+ x (fst y))) 1 2 3)
    -- [String] is a list of variables, like in example: ["x", "y"]
    -- evaluate arguments (1 2 3) and then pair them to variables (like env).
    -- so x -> 1 and y -> 2 3
    -- (Maybe String) is the CatchAll. So if there are more arguments than variables, they
    -- will go to the (Maybe String)
    -- Datum at the end is the body of the function (the thing to execute)
    -- Christa notes:
        -- You will want to 1. add constructor `FunObj [String] (Maybe String) Datum` to Datum, 
        -- where the first argument is the "normal" parameter list, the second argument is the 
        -- catch-all argument (if present), and the third is the body of the functino (the thing 
        -- to execute once you have arguments)
        -- For a parameter list like (lambda (x1 x2 . y) body), if you apply this to arguments then 
        -- there must be at least 2, and the first and second are bound as x1 and x2 in the body 
        -- if called with more arguments, everything extra gets put into a list and bound as y in the body
        -- So if called on 1 2 3 4, x1 is 1, x2 = 2, and y = (3 . (4 . ()))
    | FunctionObject [String] (Maybe String) Datum
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
