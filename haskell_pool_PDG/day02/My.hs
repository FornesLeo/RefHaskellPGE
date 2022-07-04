import Data.Maybe
import Data.Char

myElem :: Eq a => a -> [ a ] -> Bool
myElem _ [] = False
myElem n (x:xs) | n == x = True
                | otherwise = myElem n xs

safeDiv :: Int -> Int -> Maybe Int
safeDiv x 0 = Nothing
safeDiv x y = Just (x `div` y)

safeNth :: [ a ] -> Int -> Maybe a
safeNth [] y = Nothing
safeNth (x:_) 0 = Just x
safeNth (x:xs) y = safeNth xs (y - 1)

safeSucc :: Maybe Int -> Maybe Int
safeSucc Nothing = Nothing
safeSucc (Just x) = Just (x + 1)

myLookup :: Eq a => a -> [( a , b ) ] -> Maybe b
myLookup x [] = Nothing
myLookup n ((x,y):xs) | n == x = Just y
                      | otherwise = myLookup n xs

maybeDo :: ( a -> b -> c ) -> Maybe a -> Maybe b -> Maybe c
maybeDo func Nothing _ = Nothing
maybeDo func _ Nothing = Nothing
maybeDo func (Just x) (Just y) = Just (func x y)

readInt :: [ Char ] -> Maybe Int
readInt [] = Nothing
readInt ('-':xs) | all isDigit xs = Just (read (['-'] ++ xs) :: Int)
                 | otherwise = Nothing
readInt s |     all isDigit (s) = Just (read (s) :: Int)
               | otherwise = Nothing

getLineLength :: IO Int
getLineLength = do
      line <- getLine
      return $ length line

printAndGetLength :: String -> IO Int
printAndGetLength s = putStrLn s >>
              return  (length s)

printLineHorizontal :: Int -> Int -> Char -> Char -> IO ()
printLineHorizontal 1 y a b = putChar a >> putChar '\n'
printLineHorizontal x y a b | x == y = putChar a >> printLineHorizontal (x - 1) y a b
                            | otherwise = putChar b >> printLineHorizontal (x - 1) y a b

printLineMiddle :: Int -> Int -> Char -> Char -> IO ()
printLineMiddle x 2 a b = return ()
printLineMiddle x y a b = printLineHorizontal x x a b >>
                      printLineMiddle x (y - 1) a b

printBox :: Int -> IO ()
printBox 1 = printLineHorizontal (1 * 2) (1 * 2) '+' '-' >> return ()
printBox n = printLineHorizontal (n * 2) (n * 2) '+' '-' >>
             printLineMiddle (n * 2) n '|' ' ' >>
             printLineHorizontal (n * 2) (n * 2) '+' '-'

concatLines :: Int -> IO String
concatLines n | n <= 0 = return ""
              | otherwise = do
                buffer <- getLine
                line <- concatLines (n - 1)
                return (buffer ++ line)
getInt :: IO ( Maybe Int )
getInt = do
  line <- getLine
  return $ (readInt line)

main :: IO ()
main = pure ()