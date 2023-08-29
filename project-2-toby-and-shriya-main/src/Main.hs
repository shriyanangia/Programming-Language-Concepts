module Main where

import Interface
import Parser ( parseExprs )

import System.Environment (getArgs)

import Text.Parsec (runParser)
import Parser (expr, terminal)

import Control.Monad (when)
import System.IO (stdout, hSetBuffering, BufferMode(..))

--import Datum
import Printer 
--import Semantics
import AST 
import Evaluator

--Set up basic environment
--env = []
--env = [("+", Symbol "+"), ("let*", Symbol "let*")]
env = 
    --Primitive functions
    [("eq?", Symbol "eq?"), ("+", Symbol "+"), ("-", Symbol "-"), ("*", Symbol "*"), ("=", Symbol "="), 
    ("<", Symbol "<"), ("<=", Symbol "<="), (">", Symbol ">"), (">=", Symbol ">="), 
    --Special functions
    ("if", Symbol "if"), ("cond", Symbol "cond"), ("and", Symbol "and"), ("or", Symbol "or"), 
    ("let*", Symbol "let*"), ("lambda", Symbol "lambda"), ("apply", Symbol "apply"), 
    --Challenge Tasks Project 1
    ("quote", Symbol "quote"), ("eval", Symbol "eval"), ("splice", Symbol "splice"), 
    --Other functions
    ("cons", Symbol "cons"), ("fst", Symbol "fst"), ("snd", Symbol "snd"), ("list", Symbol "list"), 
    ("number?", Symbol "number?"), ("boolean?", Symbol "boolean?"), ("pair?", Symbol "pair?"), 
    ("nil?", Symbol "nil?"), ("list?", Symbol "list?")]

run :: Datum -> Maybe String
--run = fmap printDatum . eval_maybe
run x = case evaluator x env of
    Just d -> printer d
    Nothing -> Nothing

testParse :: String -> Datum
testParse s =
    case runParser (terminal (expr True)) () "" s of
        Left err -> error (show err)
        Right d -> d

main :: IO ()
main = do files <- getArgs
          if null files then repl
          else do contents <- concat <$> mapM readFile files
                  case parseExprs True contents of
                      Left err -> putStrLn ("parse error: " ++ show err)
                      Right ds -> mapM_ (\d -> case run d of
                                                   Nothing -> putStrLn "error!"
                                                   Just ress -> putStrLn ress) ds 

repl :: IO ()
repl = do hSetBuffering stdout NoBuffering
          loop
    where loop = do putStr "] "
                    s <- getLine
                    when (not (null s)) $
                        do case parseExprs True s of
                               Left err -> putStrLn ("parse error: " ++ show err)
                               Right ds -> mapM_ (\d -> case run d of
                                                            Nothing   -> putStrLn "error!"
                                                            Just ress -> putStrLn ress) ds
                           loop
