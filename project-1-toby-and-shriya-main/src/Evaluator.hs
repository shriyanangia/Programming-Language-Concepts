module Evaluator where

import AST

evaluator :: Datum -> Maybe Datum
evaluator (Number x) =  Just (Number x)
evaluator (Boolean x) = Just (Boolean x)  

--Evaluating Primitive Functions
--First element is symbol to tell you what to do, List of arguments to that 
evaluator (Cons (Symbol "eq?") args) = evaluateEq (asProperList args)
evaluator (Cons (Symbol "+") args)  = evaluateSum (numbers (evaluateList (asProperList args)))
evaluator (Cons (Symbol "-") args)  = evaluateSubtract (numbers (evaluateList (asProperList args)))
evaluator (Cons (Symbol "*") args)  = evaluateMultiply (numbers (evaluateList (asProperList args)))
evaluator (Cons (Symbol "=") args)  = evaluateEquals (numbers (evaluateList (asProperList args)))
evaluator (Cons (Symbol "<") args)  = evaluateLessThan (numbers(evaluateList(asProperList args)))
evaluator (Cons (Symbol "<=") args) = evaluateLessThanEq (numbers(evaluateList(asProperList args)))
evaluator (Cons (Symbol ">") args)  = evaluateGreaterThan (numbers(evaluateList(asProperList args)))
evaluator (Cons (Symbol ">=") args) = evaluateGreaterThanEq (numbers(evaluateList(asProperList args)))

--Evaluating Special Forms (need not evaluate all their arguments)
evaluator (Cons (Symbol "if") args) = evaluateIf (asProperList args)
evaluator (Cons (Symbol "cond") args) = evaluateCond (branches(asProperList args))
evaluator (Cons (Symbol "and") args) = evaluateAnd (asProperList args)
evaluator (Cons (Symbol "or") args) = evaluateOr ((asProperList args))

-- Other tests 
--(15: cons)
evaluator (Cons (Symbol "cons") args) = evaluateCons (evaluateList (asProperList args)) 
--evaluator (Cons (Symbol "cons") args) = evaluateCons ((asProperList args)) 

--(16: fst)
evaluator (Cons (Symbol "fst") args) = evaluateFst (evaluateList (asProperList args))
--(17: snd)
evaluator (Cons (Symbol "snd") args) = evaluateSnd (evaluateList (asProperList args))
--(18: listF)
evaluator (Cons (Symbol "list") args) = evaluateListF (evaluateList (asProperList args))
--(19: number?)
evaluator (Cons (Symbol "number?") args) = evaluateNumber (evaluateList(asProperList args))
--(20 boolean?)
evaluator (Cons (Symbol "boolean?") args) = evaluateBoolean (evaluateList(asProperList args))
--(21 pair?)
evaluator (Cons (Symbol "pair?") args) = evaluatePair (evaluateList (asProperList args))
--(21.5 nil?)
evaluator (Cons (Symbol "nil?") args) = evaluateNil (evaluateList (asProperList args))
--(22 listQ)
evaluator (Cons (Symbol "list?") args) = evaluateListQ (evaluateList (asProperList args))




-- JGM: 
--  1. Difference between special forms (if, and, or) and primitive functions (+, =, eq?)
--       - Special forms: special rules of evaluation (if may not evaluate both branches, &c.)
--       - Primitive functions: always evaluate all arguments before computing result
--  2. Consider helper functions:
--       - maybes :: [Maybe a] -> Maybe [a] If any Nothings, returns Nothing
--       - evaluateList :: [Datum] -> Maybe [Datum] Maps evaluator on each 
--       - numbers :: [Datum] -> Maybe [Integer] now have integer, can just sum 
--       - with :: Maybe a -> (a -> Maybe b) -> Maybe b - Gets rid of having to do Maybe every time


--helper functions 
numbers :: Maybe [Datum] -> Maybe [Integer]
numbers Nothing = Nothing
numbers (Just []) = Just []
numbers (Just (Number n : ds)) = helper (numbers (Just ds)) where
    helper :: Maybe [Integer] -> Maybe [Integer]
    helper Nothing = Nothing
    helper (Just ns) = Just (n:ns) --supposed to add ns this to existing list of numbers 
numbers _ = Nothing

branches :: Maybe [Datum] -> Maybe [(Datum, Datum)]
branches Nothing = Nothing 
branches (Just []) = Just []
branches (Just (Cons a b: ds)) = helper2 (branches (Just ds)) where 
    helper2 :: Maybe [(Datum, Datum)] -> Maybe [(Datum, Datum)]
    helper2 Nothing = Nothing 
    helper2 (Just ls) = Just ((a,b):ls)
branches _ = Nothing 

--evaluateList :: [Datum] -> Maybe [Datum] Maps evaluator on each 
evaluateList :: Maybe [Datum] -> Maybe [Datum]
evaluateList Nothing = Nothing
evaluateList (Just []) = Just []
evaluateList (Just ds) = sequence (map evaluator ds)

summation :: [Integer] -> Integer
summation [] = 0
summation (n:ns) = n + (summation ns)

subtraction :: [Integer] -> Integer
subtraction [] = 0
subtraction (n:ns) = n - (summation ns)

multiplication :: [Integer] -> Integer
multiplication [] = 1
multiplication (n:ns) = n * (multiplication ns)

equals :: [Integer] -> Bool
equals [x] = True
equals [x, y] = x == y
equals (x:y:zs)
    | (x == y) = (equals (y:zs))
    | otherwise = False

lessThan :: [Integer] -> Bool
lessThan [x] = True
lessThan [x, y] = x < y
lessThan (x:y:zs)
    | (x < y) = (lessThan(y:zs))
    | otherwise = False

lessThanEquals :: [Integer] -> Bool
lessThanEquals [x] = True
lessThanEquals [x, y] = (x < y) || (x == y)
lessThanEquals (x:y:zs)
    | (x < y) || (x == y) = (lessThanEquals(y:zs))
    | otherwise = False

greaterThan :: [Integer] -> Bool
greaterThan [x] = True
greaterThan [x, y] = x > y
greaterThan (x:y:zs)
    | (x > y) = (greaterThan(y:zs))
    | otherwise = False

greaterThanEquals :: [Integer] -> Bool
greaterThanEquals [x] = True
greaterThanEquals [x, y] = (x > y) || (x == y)
greaterThanEquals (x:y:zs)
    | (x > y) || (x == y) = (greaterThanEquals(y:zs))
    | otherwise = False

--Maybe [Integer] -> Maybe Integer
wrapping :: [Integer] -> Maybe [Integer]
wrapping a = Just a

unwrapInt :: Maybe [Integer] -> [Integer]
unwrapInt (Just xs) = xs

unwrapDatum :: Maybe [Datum] -> [Datum]
unwrapDatum (Just ds) = ds

unwrapDatum2 :: Maybe Datum -> Datum
unwrapDatum2 (Just d) = d

--evaluateEq
--eq? takes exactly two arguments
evaluateEq :: Maybe [Datum] -> Maybe Datum
evaluateEq (Just[d1, d2]) = Just (Boolean((show (evaluator d1))==(show (evaluator d2))))
evaluateEq (Just[d1, d2]) = Just (Boolean((show d1)==(show d2)))
evaluateEq _ = Nothing  
 
--evaluateSum
evaluateSum :: Maybe [Integer] -> Maybe Datum
evaluateSum (Just []) = Just (Number 0)
evaluateSum (Just ns) = Just (Number (summation ns))
evaluateSum Nothing = Nothing

--evaluateSubtract
evaluateSubtract :: Maybe [Integer] -> Maybe Datum
evaluateSubtract (Just []) = Nothing
evaluateSubtract (Just ns)= Just (Number (subtraction ns))
evaluateSubtract Nothing = Nothing

--evaluateMultiply
evaluateMultiply :: Maybe [Integer] -> Maybe Datum
evaluateMultiply (Just []) = Just (Number 1)
evaluateMultiply (Just ns) = Just (Number (multiplication ns))
evaluateMultiply Nothing = Nothing

--evaluateEquals
evaluateEquals :: Maybe [Integer] -> Maybe Datum
evaluateEquals (Just []) = Nothing
evaluateEquals (Just ns) = Just (Boolean (equals ns))
evaluateEquals Nothing = Nothing

--evaluateLessThan
evaluateLessThan :: Maybe [Integer] -> Maybe Datum
evaluateLessThan (Just []) = Nothing
evaluateLessThan (Just ns) = Just (Boolean (lessThan ns))
evaluateLessThan Nothing = Nothing

--evaluateLessThanEq
evaluateLessThanEq :: Maybe [Integer] -> Maybe Datum
evaluateLessThanEq (Just []) = Nothing
--evaluateLessThanEq (Just ns) = Just (Boolean ((lessThan ns) || (equals ns)))
evaluateLessThanEq (Just ns) = Just (Boolean (lessThanEquals ns))
evaluateLessThanEq Nothing = Nothing

--evaluateGreaterThan
evaluateGreaterThan :: Maybe [Integer] -> Maybe Datum
evaluateGreaterThan (Just []) = Nothing
evaluateGreaterThan (Just ns) = Just (Boolean (greaterThan ns))
evaluateGreaterThan Nothing = Nothing

--evaluateGreaterThanEq
evaluateGreaterThanEq :: Maybe [Integer] -> Maybe Datum
evaluateGreaterThanEq (Just []) = Nothing
evaluateGreaterThanEq (Just ns) = Just (Boolean (greaterThanEquals ns))
evaluateGreaterThanEq Nothing = Nothing


--Special Forms
--evaluateIf
evaluateIf :: Maybe [Datum] -> Maybe Datum
evaluateIf (Just[d1, d2, d3]) = case evaluator d1 of 
    Nothing -> Nothing 
    Just (Boolean False) -> evaluator d3
    Just (_) -> evaluator d2
evaluateIf _ = Nothing


{-evaluateCond :: Maybe [Datum] -> Maybe Datum
 evaluateCond (Just ((Cons e1 e2):ds))
    | (e1) == (Boolean False) = evaluateCond (Just ds) --If false, go to next branch
    | (e1) == Symbol "else"= (evaluateFst (Just [e2]))
    | e1 == (Boolean True) = (evaluateFst (Just [e2]))
    | otherwise = Nothing --(evaluateFst (Just [e2]))
-- | otherwise = (evaluator (unwrapDatumSingle (Just e2)))
evaluateCond _ = Nothing -}

evaluateCond :: Maybe [(Datum, Datum)] -> Maybe Datum
evaluateCond (Just []) = Nothing
evaluateCond (Just ((e1, e2) : ds)) 
    | e1 == ((Symbol "else")) = (evaluateFst(Just [e2]))
evaluateCond (Just ((e1, e2) : ds)) = case evaluator e1 of 
    (Just(Boolean True)) -> (evaluator(unwrapDatum2(evaluateFst(Just[e2]))))
    (Just(Boolean False)) -> evaluateCond (Just ds)
    Just(_) -> Nothing
evaluateCond Nothing = Nothing

--evaluateAnd
evaluateAnd :: Maybe [Datum] -> Maybe Datum
evaluateAnd (Just []) = Just (Boolean True)
evaluateAnd (Just [x]) = evaluator x
evaluateAnd (Just (x: xs)) = case evaluator x of 
    Nothing -> Nothing 
    Just (Boolean False) -> Just (Boolean False) 
    Just _ -> evaluateAnd (Just xs)
evaluateAnd _ = Nothing

--evaluateOr
evaluateOr :: Maybe [Datum] -> Maybe Datum
evaluateOr (Just []) = Just (Boolean False)
evaluateOr (Just [x]) = evaluator x
evaluateOr (Just (x: xs)) = case evaluator x of
    Nothing -> Nothing
    Just (Boolean False) -> evaluateOr (Just xs)
    Just x' -> Just x'
evaluateOr _ = Nothing

--no arguments, `(or)` evaluates to `#f`
-- If any argument evaluates to a non-`#f` value, 
--the `or ` evaluates to that value
--If any argument evaluates to a `#f`, 
--the `or` evaluates to `#f`.

--Other Tests 
evaluateCons :: Maybe [Datum] -> Maybe Datum
evaluateCons (Just[a, b]) = Just (Cons a b)
evaluateCons _ = Nothing

evaluateFst :: Maybe [Datum] -> Maybe Datum
evaluateFst (Just [Cons d1 d2]) = Just d1
evaluateFst _ = Nothing 

evaluateSnd :: Maybe [Datum] -> Maybe Datum
evaluateSnd (Just [Cons d1 d2]) = Just d2
evaluateSnd _ = Nothing 

evaluateListF :: Maybe [Datum] -> Maybe Datum
evaluateListF Nothing = Nothing
evaluateListF (Just []) = Just Nil
evaluateListF (Just (d:ds)) = Just (Cons (d) (unwrapDatumSingle (evaluateListF (Just ds))))

unwrapDatumSingle :: Maybe Datum -> Datum
unwrapDatumSingle (Just d) = d

evaluateNumber :: Maybe [Datum] -> Maybe Datum
evaluateNumber (Just [Number x]) = Just (Boolean True)
evaluateNumber (Just [d]) = Just (Boolean False)
evaluateNumber _ = Nothing

evaluateBoolean :: Maybe [Datum] -> Maybe Datum
evaluateBoolean (Just [Boolean d]) = Just (Boolean True)
evaluateBoolean (Just [d]) = Just (Boolean False)
evaluateBoolean _ = Nothing

evaluatePair :: Maybe [Datum] -> Maybe Datum
evaluatePair Nothing = Nothing
evaluatePair (Just [Cons a b]) = Just (Boolean True)
evaluatePair (Just [d]) = Just (Boolean False)
evaluatePair (_) = Nothing

evaluateNil :: Maybe [Datum] -> Maybe Datum
evaluateNil (Just [Nil]) = Just (Boolean True)
evaluateNil (Just [d]) = Just (Boolean False)
evaluateNil (_) = Nothing

evaluateListQ :: Maybe [Datum] -> Maybe Datum

evaluateListQ (Just [d]) = case asProperList d of
  Just ds -> Just (Boolean True)
  Nothing -> Just (Boolean False)
evaluateListQ _ = Nothing
