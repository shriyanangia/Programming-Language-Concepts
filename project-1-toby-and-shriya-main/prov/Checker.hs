{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Checker where

import Control.Monad ( guard )
import Test.HUnit
import Text.Parsec hiding (token)

import Interface
import Main
import Parser


instance SchemeData (Parser ()) where

    symbol s = token (string s) >> return ()

    number i = do j <- token int
                  guard (i == j)

    boolean True = token (string "#t") >> return ()    -- Careful with this one
    boolean False = token (string "#f") >> return ()

    cons d e = bracketed (d >> token (string ".") >> e)

    nil = (token (string "()") <|> token (string "[]")) >> return ()

    proper [] = nil
    proper es = bracketed (go es) where
        go [] = (token (string ".") >> nil) <|> return ()
        go (e : es) = e >> (go es <|> (token (string ".") >> proper es))

    improper [] e = e
    improper ds e = bracketed (go ds) where
        go [d] = d >> token (string ".") >> e
        go (d : ds) = d >> (go ds <|> (token (string ".") >> improper ds e))

check :: Int -> String -> Maybe String -> Assertion
check i prompt result =
    case (run d, result) of
        (Just e, Just rs) ->
            let rp :: Parser ()
                rp = case parseExprs True rs of
                         Left err -> error ("in " ++ rs ++ ": " ++ show err)
                         Right [rp] -> rp
            in case runParser (terminal rp) () "" e of
                   Left _ -> assertString (testCaseStr ++ "\nExpected \"" ++ rs ++ "\", not \"" ++ e ++ "\"")
                   Right _ -> return ()
        (Nothing, Nothing) -> return ()
        (Just e, Nothing) -> assertString (testCaseStr ++ "\nFailure expected, not \"" ++ e ++ "\"")
        (Nothing, Just rs) -> assertString (testCaseStr ++ "\nFailure unexpected, expected \"" ++ rs ++ "\"")
    where d = case parseExprs True prompt of
                 Left err -> error ("in " ++ prompt ++ ": " ++ show err)
                 Right [d] -> d
                 Right _   -> error ("wrong number of expressions: " ++ prompt)
          testCaseStr = init (unlines (("In test case [" ++ show i ++ "]:") : map ("    " ++) (lines prompt)))

check' :: String -> String -> Assertion
check' prompt result =
    case runParser (terminal rp) () "" prompt of
        Left err -> assertString ("Failed to match:\n" ++ show err)
        Right _  -> return ()
    where rp :: Parser ()
          rp = case parseExprs True result of
                 Left err -> error ("in " ++ result ++ ": " ++ show err)
                 Right [d] -> d
                 Right _   -> error ("wrong number of expressions: " ++ prompt)
          testCaseStr = init (unlines ("In test case:" : map ("    " ++) (lines prompt)))
