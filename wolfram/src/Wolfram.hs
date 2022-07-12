module Wolfram where

import Config
import Rule
import System.Exit

import Control.Concurrent


data Generation a = Generation [a] a [a]

infInt :: [Int]
infInt = [0..]

startGen :: Generation Cell
startGen = Generation [ Die | _ <- infInt ] Live [ Die | _ <- infInt ]

printNGen :: Int -> Bool -> Generation Cell -> String
printNGen n True (Generation l x r) = foldl (\ a b -> a ++ show b) "" $ reverse (take n l) ++ x : take n r
printNGen n False gen = reverse $ printNGen n True gen

nextLeft :: Rule -> Cell ->  [Cell] -> [Cell]
nextLeft r x (x':x'': xs) = r (x'', x', x) : nextLeft r x' (x'':xs)
nextLeft _ _ _ = []

nextRight :: Rule -> Cell ->  [Cell] -> [Cell]
nextRight r x (x':x'': xs) = r (x, x', x'') : nextRight r x' (x'':xs)
nextRight _ _ _ = []

nextGen :: Rule -> Generation Cell -> Generation Cell
nextGen rule (Generation l@(x:_) m r@(x':_)) = Generation (nextLeft rule m l) (rule (x, m, x')) (nextRight rule m r)
nextGen _ _ = error "pattern not found in nextGen generation"

beforeStart :: Conf -> Maybe (Generation Cell) -> Maybe (Generation Cell)
beforeStart _  Nothing = Nothing
beforeStart conf (Just gen) | confStartGen conf == 0 = Just gen
                            | otherwise = beforeStart (conf {confStartGen = confStartGen conf - 1}) (confRule conf >>= (\ a b -> Just (nextGen b a)) gen)

runWolfram :: Conf -> Maybe (Generation Cell) -> IO ()
runWolfram _ Nothing = exitWith (ExitFailure 84)
runWolfram conf (Just gen) = do
    case confTakeGen conf of
        Just 0 -> return ()
        _ -> do
            putStrLn $ printNGen (confWindow conf `div` 2) (confMove conf >= 0) gen
            threadDelay 10000
            case confRule conf of
                Just r -> runWolfram (conf {confTakeGen = confTakeGen conf >>= (\a b -> Just $ b - a) 1 }) (Just (nextGen r gen))
                Nothing -> return ()   