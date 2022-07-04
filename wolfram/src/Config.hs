module Config where

import Text.Read

type Rule = (Bool, Bool, Bool) -> Bool

data Conf = Conf { confRule :: Maybe Rule
                 , confStartGen :: Int
                 , confTakeGen :: Maybe Int
                 , confWindow :: Int
                 , confMove :: Int
                 }

defaultConf :: Conf
defaultConf = Conf Nothing 0 Nothing 80 0

rule30 :: (Bool, Bool, Bool) -> Bool
rule30 _ = False

rule90 :: (Bool, Bool, Bool) -> Bool
rule90 _ = False

rule110 :: (Bool, Bool, Bool) -> Bool
rule110 _ = False

setRule :: Int -> Maybe Rule
setRule 30 = Just rule30
setRule 90 = Just rule90
setRule 110 = Just rule110
setRule _ = Nothing

getOpt :: Conf -> String -> Int -> Maybe Conf
getOpt conf "rule" x = Just $ conf { confRule = setRule x}
getOpt conf "start" x = Just $ conf { confStartGen = x}
getOpt conf "lines" x = Just $ conf { confTakeGen = Just x}
getOpt conf "window" x = Just $ conf { confWindow = x}
getOpt conf "move" x = Just $ conf { confMove = x}
getOpt _ _ _ = Nothing

getOpts :: [String] -> Conf -> Maybe Conf
getOpts [] conf = Just conf
getOpts (('-':'-':cmd):arg:xs) conf = (readMaybe arg :: Maybe Int) >>= getOpt conf cmd  >>= getOpts xs
getOpts _ _ = Nothing