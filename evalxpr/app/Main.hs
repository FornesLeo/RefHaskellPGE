module Main where

import System.Exit             
import System.Environment (getArgs)
import Text.Printf

import Evalxpr

displayResult :: Maybe Double -> IO ()
displayResult Nothing = exitWith (ExitFailure 84)
displayResult (Just x) = printf "%.2f\n" x


main :: IO ()
main = do
    argv <- getArgs
    case argv of
        []  -> exitWith (ExitFailure 84)
        [x] -> displayResult $ runEvalxpr x
        _   -> exitWith (ExitFailure 84)
