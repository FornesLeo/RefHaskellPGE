module Config where

import Text.Read

import Rule

data Conf = Conf { confRule :: Maybe Rule
                 , confStartGen :: Int
                 , confTakeGen :: Maybe Int
                 , confWindow :: Int
                 , confMove :: Int
                 }

defaultConf :: Conf
defaultConf = Conf Nothing 0 Nothing 80 0

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