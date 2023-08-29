module Main where

import Interface
import Parser ( parseExprs )

import Control.Monad (when)
import System.Environment (getArgs)
import System.IO (stdout, hSetBuffering, BufferMode(..))

import Datum
import Semantics ( eval_maybe )
import Printer

import Text.Parsec (runParser)
import Parser (expr, terminal)

testParse :: String -> Datum
testParse s =
    case runParser (terminal (expr True)) () "" s of
        Left err -> error (show err)
        Right d -> d

run :: Datum -> Maybe String
run = fmap printDatum . eval_maybe

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
