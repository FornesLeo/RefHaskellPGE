module Main where

import Config
import Wolfram

import System.Exit
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case getOpts args defaultConf of
        Nothing -> exitWith (ExitFailure 84)
        Just Conf { confRule = Nothing,
                    confStartGen = _,
                    confTakeGen = _,
                    confWindow = _,
                    confMove = _ } -> exitWith (ExitFailure 84)
        Just conf -> runWolfram conf
