import Data.Char
import Data.Maybe
import System.Exit
import System.Environment

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem x (y:ys) = case x == y of
    True -> True
    False -> myElem x ys

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

safeNth :: [a] -> Int -> Maybe a
safeNth [] _  = Nothing
safeNth (x:_) 0 = Just x
safeNth (_:xs) y = safeNth xs (y - 1)

safeSucc :: Maybe Int -> Maybe Int
safeSucc Nothing = Nothing
safeSucc (Just x) = Just $ x + 1

safeSucc1 :: Maybe Int -> Maybe Int
safeSucc1 x = fmap (\z -> z + 1) x

safeSucc2 :: Maybe Int -> Maybe Int
safeSucc2 x =  x >>= (\z -> Just (z + 1))

myLookup :: Eq a => a -> [(a, b)] -> Maybe b
myLookup _ [] = Nothing
myLookup x (y:ys) = case x == fst y of
    True -> Just $ snd y
    False -> myLookup x ys

maybeDo :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
maybeDo _ _ Nothing =  Nothing
maybeDo _ Nothing _ =  Nothing
maybeDo f (Just x) (Just y) =  Just (f x y)

maybeDo1 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
maybeDo1 f x y = f <$> x <*> y

isOnlyDigit :: [Char] -> Bool
isOnlyDigit [] = True
isOnlyDigit (x:xs) =  case isDigit x of
    False -> False
    True -> isOnlyDigit xs

readInt :: [Char] -> Maybe Int
readInt x = case isOnlyDigit x of
    False -> Nothing
    True -> Just (read x :: Int)

getLineLength :: IO Int
getLineLength = do
    line <- getLine
    return $ length line

printAndGetLength :: String -> IO Int
printAndGetLength x = do
    print x
    return $ length x

printTop :: Int -> IO ()
printTop x = putStrLn $ "+" ++ (take (x - 2) (repeat '-')) ++ "+"

printMiddle :: Int -> Int -> IO ()
printMiddle x y | y <= 0 = pure ()
                | otherwise = do
                    printMiddle x (y - 1)
                    putStrLn $ "|" ++ (take (x - 2) (repeat ' ')) ++ "|"

printBox :: Int -> IO ()
printBox x  | x <= 0 = pure ()
            | otherwise = do
                printTop (x * 2)
                printMiddle (x * 2) (x - 2)
                printTop (x * 2)


concatLines :: Int -> IO String
concatLines x   | x <= 0 = return ""
                | otherwise = do
                    line <- getLine
                    result <- concatLines (x - 1)
                    return $ line ++ result

getInt :: IO ( Maybe Int )
getInt = do
    line <- getLine
    return $ readInt line

exitFail :: IO ()
exitFail = exitWith (ExitFailure 84)

doOp :: Int -> String -> Int -> Maybe   Int
doOp x "+" y = Just (x + y)
doOp x "-" y = Just (x - y)
doOp x "*" y = Just (x * y)
doOp x "/" y = Just (x `div` y)
doOp x "%" y = Just (x `mod` y)
doOp _ _ _ = Nothing

main :: IO ()
main = do
    args <- getArgs
    case (length args) == 3 of
        False -> exitFail
        True -> case (readInt (head args), (safeNth args 1), readInt (last args)) of
            (Nothing, _, _) -> exitFail
            (_, Nothing, _) -> exitFail
            (_, _, Nothing) -> exitFail
            ((Just x), (Just y), (Just z)) -> case doOp x y z of
                Nothing -> exitFail
                (Just x) -> putStr $ show $ x
