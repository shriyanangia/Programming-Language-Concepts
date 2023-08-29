{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use isNothing" #-}
{-# HLINT ignore "Use mapM" #-}
{-# HLINT ignore "Use foldr" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# HLINT ignore "Replace case with maybe" #-}
module Evaluator where

import AST
import Data.Bits (Bits(xor))

specialFormList = ["if", "cond", "and", "or", "let*", "lambda"]
specialFormJustSymbolList = [Just (Symbol "if"), Just (Symbol "cond"), Just (Symbol "and"), 
    Just( Symbol "or"), Just (Symbol "let*"), Just (Symbol "lambda")]

evaluator :: Datum -> [(String, Datum)] -> Maybe Datum
evaluator (Number x) env = Just (Number x)
evaluator (Boolean x) env = Just (Boolean x)

evaluator (Symbol s) env = case lookup s env of
    Nothing -> Nothing
    Just s' ->  Just s'

evaluator (Cons (Symbol s) args) env -- = case lookup s env of
    | (s `notElem` specialFormList) && ((lookup s env) `elem` specialFormJustSymbolList) = Nothing
    | otherwise = case lookup s env of
    --evaluating primitive functions (evaluate all arguments)
    Just (Symbol "eq?") -> evaluateEq (asProperList args) env
    Just (Symbol "+") -> evaluateSum (numbers (evaluateList (asProperList args)env)env) env
    Just (Symbol "-") -> evaluateSubtract (numbers (evaluateList (asProperList args)env)env) env
    Just (Symbol "*") -> evaluateMultiply (numbers (evaluateList (asProperList args)env)env) env
    Just (Symbol "=") -> evaluateEquals (numbers (evaluateList (asProperList args)env)env) env
    Just (Symbol "<") -> evaluateLessThan (numbers(evaluateList(asProperList args)env)env) env
    Just (Symbol "<=") -> evaluateLessThanEq (numbers(evaluateList(asProperList args)env)env) env
    Just (Symbol ">") -> evaluateGreaterThan (numbers(evaluateList(asProperList args)env)env) env
    Just (Symbol ">=") -> evaluateGreaterThanEq (numbers(evaluateList(asProperList args)env)env) env
    --Project 2
    Just (Symbol "apply") -> evaluateApply (asProperList args) env
    --Just (Symbol "apply") -> evaluateApply (evaluateList (asProperList args) env) env

    --evaluating specialforms (need not evaluate all their arguments)
    Just (Symbol "if") -> evaluateIf (asProperList args) env
    Just (Symbol "cond") -> evaluateCond (branches(asProperList args)) env
    Just (Symbol "and") -> evaluateAnd (asProperList args) env
    Just (Symbol "or") -> evaluateOr (asProperList args) env

    --Project 2
    Just (Symbol "let*") -> evaluateLetStar (asProperList args) env
    Just (Symbol "lambda") -> evaluateLambda (asProperList args) env
    --Just (Cons (Symbol "lambda") (params)) -> evaluateLambda (asProperList params) env

    --Challenge Tasks Project 1
    Just (Symbol "quote") -> evaluateQuote (asProperList args) env
    Just (Symbol "eval") -> evaluateEval (asProperList args) env
    --Just (Symbol "splice") -> evaluateSplice (asProperList args) env

    --Other tests
    Just (Symbol "cons") -> evaluateCons (evaluateList (asProperList args) env) env
    Just (Symbol "fst") -> evaluateFst (evaluateList (asProperList args) env) env
    Just (Symbol "snd") -> evaluateSnd (evaluateList (asProperList args) env) env
    Just (Symbol "list") -> evaluateListF (evaluateList (asProperList args) env) env
    Just (Symbol "number?") -> evaluateNumber (evaluateList(asProperList args) env) env
    Just (Symbol "boolean?") -> evaluateBoolean (evaluateList(asProperList args) env) env
    Just (Symbol "pair?") -> evaluatePair (evaluateList (asProperList args) env) env
    Just (Symbol "nil?") -> evaluateNil (evaluateList (asProperList args) env) env
    Just (Symbol "list?") -> evaluateListQ (evaluateList (asProperList args) env) env

    _ -> Nothing

evaluator (FunctionObject params catchAll body) env = evaluateFunctionObject (FunctionObject params catchAll body) env
evaluator (Cons f args) env = case evaluator f env of
    Nothing -> Nothing
    f' -> evaluator (Cons (unwrapDatumSingle f') args) env




--evaluator (Cons (Cons (Symbol "lambda") params) body) env = evaluateLambda (Just [params, body]) env

{-
--Evaluating Primitive Functions
--First element is symbol to tell you what to do, List of arguments to that 
evaluator (Cons (Symbol "eq?") args) env = evaluateEq (asProperList args) env 
evaluator (Cons (Symbol "+") args) env  = evaluateSum (numbers (evaluateList (asProperList args)env)env) env 
evaluator (Cons (Symbol "-") args) env  = evaluateSubtract (numbers (evaluateList (asProperList args)env)env) env
evaluator (Cons (Symbol "*") args) env  = evaluateMultiply (numbers (evaluateList (asProperList args)env)env) env
evaluator (Cons (Symbol "=") args) env  = evaluateEquals (numbers (evaluateList (asProperList args)env)env) env
evaluator (Cons (Symbol "<") args) env  = evaluateLessThan (numbers(evaluateList(asProperList args)env)env) env
evaluator (Cons (Symbol "<=") args) env = evaluateLessThanEq (numbers(evaluateList(asProperList args)env)env) env
evaluator (Cons (Symbol ">") args) env  = evaluateGreaterThan (numbers(evaluateList(asProperList args)env)env) env
evaluator (Cons (Symbol ">=") args) env = evaluateGreaterThanEq (numbers(evaluateList(asProperList args)env)env) env

--Evaluating Special Forms (need not evaluate all their arguments)
evaluator (Cons (Symbol "if") args) env = evaluateIf (asProperList args) env
evaluator (Cons (Symbol "cond") args) env = evaluateCond (branches(asProperList args)) env
evaluator (Cons (Symbol "and") args) env = evaluateAnd (asProperList args) env
evaluator (Cons (Symbol "or") args) env = evaluateOr (asProperList args) env

evaluator (Cons (Symbol "let*") args) env = evaluateLetStar (asProperList args) env

--evaluator (Cons (Symbol "lambda") args) env = evaluateLambda (asProperList args) env

evaluator (Cons (Symbol "apply") args) env = evaluator args env

-- Challenge Tasks Project 1
evaluator (Cons (Symbol "quote") args) env = evaluateQuote (asProperList args) env
--evaluator (Cons (Symbol "quote") args) env = Just args

-- Other tests 
--(15: cons)
evaluator (Cons (Symbol "cons") args) env = evaluateCons (evaluateList (asProperList args) env) env
--(16: fst)
evaluator (Cons (Symbol "fst") args) env = evaluateFst (evaluateList (asProperList args) env) env
--(17: snd)
evaluator (Cons (Symbol "snd") args) env = evaluateSnd (evaluateList (asProperList args) env) env
--(18: listF)
evaluator (Cons (Symbol "list") args) env = evaluateListF (evaluateList (asProperList args) env) env
--(19: number?)
evaluator (Cons (Symbol "number?") args) env = evaluateNumber (evaluateList(asProperList args) env) env
--(20 boolean?)
evaluator (Cons (Symbol "boolean?") args) env = evaluateBoolean (evaluateList(asProperList args) env) env
--(21 pair?)
evaluator (Cons (Symbol "pair?") args) env = evaluatePair (evaluateList (asProperList args) env) env
--(21.5 nil?)
evaluator (Cons (Symbol "nil?") args) env = evaluateNil (evaluateList (asProperList args) env) env
--(22 listQ)
evaluator (Cons (Symbol "list?") args) env = evaluateListQ (evaluateList (asProperList args) env) env

-}

--helper functions 
--numbers :: Maybe [Datum] -> [(String, Datum)] -> Maybe [Integer]
numbers :: Maybe [Datum] -> [(String, Datum)] -> Maybe [Integer]
numbers Nothing env = Nothing
numbers (Just []) env = Just []
numbers (Just (Number n : ds)) env = helper (numbers (Just ds) env) where
    helper :: Maybe [Integer] -> Maybe [Integer]
    helper Nothing = Nothing
    helper (Just ns) = Just (n:ns) --supposed to add ns this to existing list of numbers 
numbers _ env = Nothing

branches :: Maybe [Datum] -> Maybe [(Datum, Datum)]
branches Nothing = Nothing
branches (Just []) = Just []
branches (Just (Cons a b: ds)) = helper2 (branches (Just ds)) where
    helper2 :: Maybe [(Datum, Datum)] -> Maybe [(Datum, Datum)]
    helper2 Nothing = Nothing
    helper2 (Just ls) = Just ((a,b):ls)
branches _ = Nothing

evaluateList :: Maybe [Datum] -> [(String, Datum)] -> Maybe [Datum]
evaluateList Nothing env = Nothing
evaluateList (Just []) env = Just []
evaluateList (Just ds) env = sequence (map ((flip evaluator) env) ds)

summation :: [Integer] -> Integer
summation [] = 0
summation (n:ns) = n + summation ns

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

unwrapDatum3 :: Maybe[(String, Datum)] -> [(String, Datum)]
unwrapDatum3 (Just ds) = ds

unwrapDatumGeneral :: Maybe x -> x
unwrapDatumGeneral (Just x) = x

--evaluateEq
--eq? takes exactly two arguments
evaluateEq :: Maybe [Datum] -> [(String, Datum)] -> Maybe Datum
evaluateEq (Just[d1, d2]) env = Just (Boolean((show (evaluator d1 env))==(show (evaluator d2 env))))
evaluateEq _ env = Nothing

--evaluateSum
evaluateSum :: Maybe [Integer] -> [(String, Datum)] -> Maybe Datum
evaluateSum (Just []) env = Just (Number 0)
evaluateSum (Just ns) env = Just (Number (summation ns))
evaluateSum Nothing env = Nothing

--evaluateSubtract
evaluateSubtract :: Maybe [Integer] -> [(String, Datum)] -> Maybe Datum
evaluateSubtract (Just []) env = Nothing
evaluateSubtract (Just ns) env = Just (Number (subtraction ns))
evaluateSubtract Nothing env = Nothing

--evaluateMultiply
evaluateMultiply :: Maybe [Integer] -> [(String, Datum)] -> Maybe Datum
evaluateMultiply (Just []) env = Just (Number 1)
evaluateMultiply (Just ns) env = Just (Number (multiplication ns))
evaluateMultiply Nothing env = Nothing

--evaluateEquals
evaluateEquals :: Maybe [Integer] -> [(String, Datum)] -> Maybe Datum
evaluateEquals (Just []) env = Nothing
evaluateEquals (Just ns) env = Just (Boolean (equals ns))
evaluateEquals Nothing env = Nothing

--evaluateLessThan
evaluateLessThan :: Maybe [Integer] -> [(String, Datum)] -> Maybe Datum
evaluateLessThan (Just []) env = Nothing
evaluateLessThan (Just ns) env = Just (Boolean (lessThan ns))
evaluateLessThan Nothing env = Nothing

--evaluateLessThanEq
evaluateLessThanEq :: Maybe [Integer] -> [(String, Datum)] -> Maybe Datum
evaluateLessThanEq (Just []) env = Nothing
evaluateLessThanEq (Just ns) env = Just (Boolean (lessThanEquals ns))
evaluateLessThanEq Nothing env = Nothing

--evaluateGreaterThan
evaluateGreaterThan :: Maybe [Integer] -> [(String, Datum)] -> Maybe Datum
evaluateGreaterThan (Just []) env = Nothing
evaluateGreaterThan (Just ns) env = Just (Boolean (greaterThan ns))
evaluateGreaterThan Nothing env = Nothing

--evaluateGreaterThanEq
evaluateGreaterThanEq :: Maybe [Integer] -> [(String, Datum)] -> Maybe Datum
evaluateGreaterThanEq (Just []) env = Nothing
evaluateGreaterThanEq (Just ns) env = Just (Boolean (greaterThanEquals ns))
evaluateGreaterThanEq Nothing env = Nothing


--Special Forms

--letStar :: Maybe [let*, binds, body] -> env -> Maybe Datum
--the first let* is removed, though, so we only have [binds, body]
--binds is a list of (String, Datum)

--For test case 339: and can't be bound to f because it isn't a primitive function.
--need to fix that somehow in this function.


--letStar
evaluateLetStar :: Maybe [Datum] -> [(String, Datum)] -> Maybe Datum
evaluateLetStar Nothing env = Nothing
evaluateLetStar (Just [bindings, body]) env = case asProperList bindings of
    Nothing -> Nothing
    Just bindings'  --evaluator (body) (unwrapDatum3 (updateEnv bindings' env))
        | (updateEnv bindings' env) == Nothing -> Nothing
        | otherwise -> evaluator (body) (unwrapDatum3 (updateEnv bindings' env))
        where   updateEnv :: [Datum] -> [(String, Datum)] -> Maybe [(String, Datum)]
                updateEnv [] env = Just env
                updateEnv (b:bs) env = case asProperList b of
                    Nothing -> Nothing
                    (Just[Symbol b1, b2]) -> case evaluator b2 env of
                        Nothing -> Nothing
                        Just b2' -> updateEnv bs ((b1, b2') : env)
                    _ -> Nothing
evaluateLetStar _ env = Nothing


unwrapString :: Maybe String -> String
unwrapString (Just s) = s

{-
evaluateLambda :: Maybe [Datum] -> [(String, Datum)] -> Maybe Datum
evaluateLambda Nothing env = Nothing
 
evaluateLambda (Just [params, body]) env = evaluator (uncurry FunctionObject (unwrapDatumGeneral (checkParams params)) body) env
    where   checkParams :: Datum -> Maybe ([String], Maybe String)
            checkParams (Symbol x) = Just ([x], Nothing)
            checkParams (Cons (Symbol y) (Nil)) = Just ([y], Just y)
            checkParams (Cons (Symbol y) (rest)) = Just (y : (fst (unwrapDatumGeneral (checkParams rest))), Just y)
            checkParams _ = Just ([], Nothing)

evaluateLambda _ env = Nothing
-}
--Also, if checkParams fails, then evaluateLambda should also fail, 
--but right now you are assuming it always succeeds by having it return Just of something

evaluateLambda :: Maybe [Datum] -> [(String, Datum)] -> Maybe Datum
evaluateLambda Nothing env = Nothing
evaluateLambda (Just [params, body]) env = case checkParams params of
    Nothing -> Nothing
    returns -> evaluator (uncurry FunctionObject (unwrapDatumGeneral returns) body) env
evaluateLambda _ env = Nothing 

checkParams :: Datum -> Maybe ([String], Maybe String)
checkParams Nil = Just ([], Nothing)
checkParams (Symbol s) = Just ([], Just s)
checkParams (Cons (Symbol s) rest) = case checkParams rest of
    Nothing -> Nothing
    Just (params, catchAll) -> Just (s : params, catchAll)
checkParams _ = Nothing 

{-
evaluateLambda :: Maybe [Datum] -> [(String, Datum)] -> Maybe Datum
evaluateLambda Nothing env = Nothing

evaluateLambda (Just [params, body]) env = Just (uncurry FunctionObject (unwrapDatumGeneral (checkParams params)) body)
    where   checkParams :: Datum -> Maybe ([String], Maybe String)
            checkParams (Symbol x) = Just ([x], Nothing)
            checkParams (Cons (Symbol y) (Nil))
            --if parameter == body
                | length (asProperList params) == length (asProperList body) = Just ([y], Just y)
                | otherwise = Nothing 
            checkParams (Cons (Symbol y) (rest)) 
            --if one less parameter (catch all)
                | (length (asProperList params)) + 1 == length (asProperList body) = case (lastElem (unwrapDatumGeneral(asProperList params))) of 
                    last -> Just ([take (length (asProperList params)) (show params)], (collectArgs [body]))
                    --[String] (Maybe String) Datum
                    --Just (y : (fst (unwrapDatumGeneral (checkParams rest))), Just y)
                | otherwise = Nothing 
                where   collectArgs :: [Datum] -> Maybe String
                        collectArgs [d] = Just (show d)
                        collectArgs (d:ds) = Just ((show d) ++ " " ++ (unwrapString (collectArgs ds)))
            checkParams _ = Just ([], Nothing)

evaluateLambda _ env = Nothing 
-}

lastElem :: [Datum] -> Maybe Datum 
lastElem [x] = Just x --base case is when there's just one element remaining
lastElem (_:xs) = lastElem xs --if there's anything in the head, continue until there's one element left
lastElem [] = Nothing 

--FunctionObjects args catchAll (evaluator body env)
{- 
evaluateLambda (Just (x:ys)) env = Just (FunctionObject _ _ (unwrapDatumSingle (evaluator (head ys) (updateEnv x ys env))))
    where   updateEnv :: Datum -> [Datum] -> [(String, Datum)] -> Maybe [(String, Datum)]
            updateEnv _ [] env = Just env
            updateEnv d (b:bs) env = case asProperList d of
                Nothing -> Nothing
                Just d' -> Just (updateEnv Right (d') bs [(Left d', b)])
 -}



{- evaluateLambda (Just ((Cons (variables) (returned)) : args)) env = evaluator (returned) (unwrapDatum3 (updateEnv (Just variables) args env))
    where   updateEnv :: Maybe Datum -> [Datum] -> [(String, Datum)] -> Maybe [(String, Datum)]
            updateEnv Nothing [] env = Just env
            updateEnv (Just (Cons (Symbol v) (right) )) (b:bs) env = updateEnv (Just right) bs ((v, b) : env)
            updateEnv _ _ _ = Just env

evaluateLambda _ env = Nothing -}

{- 

evaluateLambda (x: xs) env = case (evaluator x env) of 
    Nothing -> Nothing 
    Right (Symbol s)                           -> _ 
    Right (FunctionObject args catchAll body) -> _ 
    Right _                                    -> Nothing
    Left _                                     -> Nothing  
        where 
            collectArgs :: [Datum] -> Either Nothing [Datum]
            collectArgs [] = Right []
            collectArgs [x] = 
                case evaluator x env of 
                    Left e -> Left e
                    Right expr -> case (evaluator x env) of
                        Nothing -> Nothing  
                        --(not(asProperList x'))   -> Left Nothing
                        _                   -> Right x    
            collectArgs (x:xs) = evaluator x env 
 -}
--FunctionObjects args catchAll (evaluator body env)
--("((lambda (x) x) 1)", Just "1")

evaluateFunctionObject :: Datum -> [(String, Datum)] -> Maybe Datum
evaluateFunctionObject (FunctionObject params catchAll body) env = Just (body) --not sure what to do here

evaluateApply :: Maybe [Datum] -> [(String, Datum)] -> Maybe Datum
evaluateApply Nothing env = Nothing 

--evaluateApply (Just f) env = evaluator (properList f) env
{- evaluateApply (Just f) env = evaluator (properList ((take (length f) f) ++ [unwrapDatumSingle (evaluateLast f)])) env
    where   evaluateLast :: [Datum] -> Maybe Datum 
            evaluateLast [d] = evaluator d env
            evaluateLast (d:ds) = evaluateLast ds  -}
--evaluateApply (Just f) env = evaluator (properList (unwrapDatumGeneral (evaluateList (Just f) env))) env

{- 
evaluateApply (Just (x:xs)) env = case (evaluator x env) of 
    Nothing -> Nothing 
    Just x' -> case (collectArgs xs) of 
      Nothing -> Nothing
      Just das -> Nothing
      where 
            collectArgs :: [Datum] -> Maybe [Datum]
            collectArgs [] = Just []
            collectArgs [x] = case evaluator x env of 
                Nothing -> Nothing
                x' -> Just [unwrapDatumGeneral x']
-}
evaluateApply _ env = Nothing
 
{-
evaluateApply (x: xs) env = case (evaluator x env) of 
    Nothing -> Nothing 
    Right (Symbol s)                           -> _ 
    Right (FunctionObjects args catchAll body) -> _ 
    Right _                                    -> Nothing
    Left _                                     -> Nothing  
        where 
            collectArgs :: [Datum] -> Either Error [Datum]
            collectArgs [] = Right []
            collectArgs [x] = 
                case evaluator x env of 
                    Left e -> Left e
                    Right expr -> case (evaluator x env) of
                        Nothing -> Nothing  
                        --(not(asProperList x'))   -> Left Nothing
                        _                   -> Right x    
            collectArgs (x:xs) = evaluator x env 
-}


--evaluateIf
evaluateIf :: Maybe [Datum] -> [(String, Datum)] -> Maybe Datum
evaluateIf (Just[d1, d2, d3]) env = case evaluator d1 env of
    Nothing -> Nothing
    Just (Boolean False) -> evaluator d3 env
    Just (_) -> evaluator d2 env
evaluateIf _ env = Nothing

--evaluateCond
evaluateCond :: Maybe [(Datum, Datum)] -> [(String, Datum)] -> Maybe Datum
evaluateCond (Just []) env = Nothing
evaluateCond (Just ((e1, e2) : ds)) env
    | e1 == ((Symbol "else")) = (evaluateFst(Just [e2]) env)
evaluateCond (Just ((e1, e2) : ds)) env = case evaluator e1 env of
    (Just(Boolean True)) -> (evaluator(unwrapDatum2(evaluateFst(Just[e2]) env)) env)
    (Just(Boolean False)) -> evaluateCond (Just ds) env
    _ -> Nothing
evaluateCond Nothing env = Nothing

--evaluateAnd
evaluateAnd :: Maybe [Datum] -> [(String, Datum)] -> Maybe Datum
evaluateAnd (Just []) env = Just (Boolean True)
evaluateAnd (Just [x]) env = evaluator x env
evaluateAnd (Just (x: xs)) env = case evaluator x env of
    Nothing -> Nothing
    Just (Boolean False) -> Just (Boolean False)
    Just _ -> evaluateAnd (Just xs) env
evaluateAnd _ env = Nothing

--evaluateOr
evaluateOr :: Maybe [Datum] -> [(String, Datum)] -> Maybe Datum
evaluateOr (Just []) env = Just (Boolean False)
evaluateOr (Just [x]) env = evaluator x env
evaluateOr (Just (x: xs)) env = case evaluator x env of
    Nothing -> Nothing
    Just (Boolean False) -> evaluateOr (Just xs) env
    Just x' -> Just x'
evaluateOr _ env = Nothing

--Challenge tasks Problem 1 Special Form
evaluateQuote :: Maybe [Datum] -> [(String, Datum)] -> Maybe Datum
--evaluateQuote (Just [Cons (Symbol "splice") (Cons (arg) (_))]) env = evaluateSplice (Just [arg]) env
evaluateQuote (Just [Cons a b]) env
    | a == Symbol "splice" = evaluateSplice (Just b) env 
    -- (splice #t) --> Cons (Symbol "splice") (Cons (Boolean True) Nil)) --> so calling evaluateSplice on **Cons (Boolean True) Nil**
    -- replace (Just b) with argument to evaluateSplice
    | b == Nil = Just (Cons a b)
    | otherwise = Just (Cons a (unwrapDatumGeneral (evaluateQuote (Just [b]) env))) --evaluateQuote (Just [b]) env
evaluateQuote (Just [d]) env = Just d
evaluateQuote _ env = Nothing

evaluateEval :: Maybe [Datum] -> [(String, Datum)] -> Maybe Datum
evaluateEval (Just [d]) env = evaluator d env
evaluateEval _ env = Nothing 

evaluateSplice :: Maybe Datum -> [(String, Datum)] -> Maybe Datum
--evaluateSplice = undefined
evaluateSplice (Just (Cons a Nil)) env = Just ((unwrapDatumGeneral (evaluator a env)) )
evaluateSplice (Just (Cons a b)) env = Just (Cons (unwrapDatumGeneral (evaluator a env)) (unwrapDatumGeneral (evaluator b env)))
--evaluateSplice (Just (d:ds)) env = evaluator d env : evaluateSplice (Just ds) env
evaluateSplice _ env = Nothing

--Other Tests 
evaluateCons :: Maybe [Datum] -> [(String, Datum)] -> Maybe Datum
evaluateCons (Just[a, b]) env = Just (Cons a b)
evaluateCons _ env = Nothing

evaluateFst :: Maybe [Datum] -> [(String, Datum)] -> Maybe Datum
evaluateFst (Just [Cons d1 d2]) env = Just d1
evaluateFst _ env = Nothing

evaluateSnd :: Maybe [Datum] -> [(String, Datum)] -> Maybe Datum
evaluateSnd (Just [Cons d1 d2]) env = Just d2
evaluateSnd _ env = Nothing

evaluateListF :: Maybe [Datum] -> [(String, Datum)] -> Maybe Datum
evaluateListF Nothing env = Nothing
evaluateListF (Just []) env = Just Nil
evaluateListF (Just (d:ds)) env = Just (Cons (d) (unwrapDatumSingle (evaluateListF (Just ds) env)))

unwrapDatumSingle :: Maybe Datum -> Datum
unwrapDatumSingle (Just d) = d

evaluateNumber :: Maybe [Datum] -> [(String, Datum)] -> Maybe Datum
evaluateNumber (Just [Number x]) env = Just (Boolean True)
evaluateNumber (Just [d]) env = Just (Boolean False)
evaluateNumber _ env = Nothing

evaluateBoolean :: Maybe [Datum] -> [(String, Datum)] -> Maybe Datum
evaluateBoolean (Just [Boolean d]) env = Just (Boolean True)
evaluateBoolean (Just [d]) env = Just (Boolean False)
evaluateBoolean _ env = Nothing

evaluatePair :: Maybe [Datum] -> [(String, Datum)] -> Maybe Datum
evaluatePair Nothing env = Nothing
evaluatePair (Just [Cons a b]) env = Just (Boolean True)
evaluatePair (Just [d]) env = Just (Boolean False)
evaluatePair (_) env = Nothing

evaluateNil :: Maybe [Datum] -> [(String, Datum)] -> Maybe Datum
evaluateNil (Just [Nil]) env = Just (Boolean True)
evaluateNil (Just [d]) env = Just (Boolean False)
evaluateNil (_) env = Nothing

evaluateListQ :: Maybe [Datum] -> [(String, Datum)] -> Maybe Datum
evaluateListQ (Just [d]) env = case asProperList d of
  Just ds -> Just (Boolean True)
  Nothing -> Just (Boolean False)
evaluateListQ _ env = Nothing
