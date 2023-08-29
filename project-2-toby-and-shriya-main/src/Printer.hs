{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Printer where

import AST
import Bird.Printer

printer :: Datum -> Maybe String
printer (Number x) = Just (show x)
printer (Boolean True)  = Just ("#t")
printer (Boolean False) = Just ("#f")

printer (Symbol a) = Just (a)

printer ((Cons x y)) = Just ( "(" ++ (wrappingString(printer x)) ++ " . " ++ (wrappingString(printer y)) ++ ")" )
--cons printer is correct 

printer (FunctionObject [s] (ms) d) = Just (show (FunctionObject [s] (ms) d))

printer (Nil) = Just "()"

printer _ = Nothing


--helper functions 
wrappingString :: Maybe String -> String
wrappingString (Just (x:xs)) =  x: xs
 
