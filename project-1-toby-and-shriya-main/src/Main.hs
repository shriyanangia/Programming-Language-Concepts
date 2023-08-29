module Main where

import Interface
import Parser ( parseExprs )
import AST
import Evaluator
import Printer

import System.Environment (getArgs)

--tester imports
import Text.Parsec (runParser)
import Parser (expr, terminal)

--tester
testParse :: String -> Datum
testParse s =
    case runParser (terminal (expr True)) () "" s of
        Left err -> error (show err)
        Right d -> d

-- >>> testParse "(+ 1 2 3)"
-- Cons (Symbol "+") (Cons (Number 1) (Cons (Number 2) (Cons (Number 3) Nil)))
-- Then have to run asProperList

run :: Datum -> Maybe String
run x = case (evaluator ((x))) of
    Just d -> printer d
    Nothing -> Nothing

main :: IO ()
main = do files <- getArgs
          contents <- concat <$> mapM readFile files
          case parseExprs True contents of
              Left err -> putStrLn ("parse error: " ++ show err)
              Right ds -> mapM_ (\d -> case run d of
                                           Nothing -> putStrLn "error"
                                           Just ress -> putStrLn ress) ds
 
