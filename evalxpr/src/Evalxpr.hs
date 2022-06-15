module Evalxpr where

import Parser
import Type

removeSpace :: String -> String
removeSpace [] = []
removeSpace (' ':xs) = removeSpace xs
removeSpace (x:xs) =  x:removeSpace xs

runEvalxpr :: String -> Maybe Double
runEvalxpr str = case runParser parseExpr (removeSpace str) of
            Just (x, []) -> Just (eval x)
            Just (_, y)  -> Nothing
            Nothing      -> Nothing