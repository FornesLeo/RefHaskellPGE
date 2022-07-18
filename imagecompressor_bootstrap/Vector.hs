import Control.Applicative
import System.Environment (getArgs)
import Data.Maybe

type Vector = (Double, Double, Double)

distance :: Vector -> Vector -> Double
distance (x, x', x'') (y, y', y'') = sqrt $ (x - y) ^ 2 + (x' - y') ^ 2 + (x'' - y'') ^ 2

data Parser a = Parser {
    runParser :: String -> Maybe (a , String )
}

instance Functor Parser where
    fmap f p = Parser $ \s -> case runParser p s of
        Nothing -> Nothing
        Just (a, s') -> Just (f a, s')

instance Applicative Parser where
    pure x = Parser $ \s -> Just (x, s)

    (<*>) p1 p2 = Parser $ \s -> case runParser p1 s of
        Nothing -> Nothing
        Just (f, s') -> case runParser p2 s' of
            Nothing       -> Nothing
            Just (n, s'') -> Just (f n, s'')
    (<*) p1 p2 = const <$> p1 <*> p2
    (*>) p1 p2 = flip const <$> p1 <*> p2

instance Alternative Parser where
    p1 <|> p2 = Parser $ \s -> case runParser p1 s of
        Nothing -> runParser p2 s
        Just (r, s') -> Just (r, s')

    empty = Parser $ const Nothing

instance Monad Parser where
    (>>=) p f = Parser $ \s -> case runParser p s of
        Nothing -> Nothing
        Just (r, s') -> runParser (f r) s'


parsePred :: (Char -> Bool) -> Parser Char
parsePred f = Parser $ \ s -> case s of
  [] -> Nothing
  (x:xs) | f x -> Just (x, xs)
         | otherwise -> Nothing

parseChar :: Char -> Parser Char
parseChar x = parsePred (==x)

parseAnyChar :: String -> Parser Char
parseAnyChar x = parsePred (`elem` x)

parseMany :: Parser a -> Parser [a]
parseMany p = (:) <$> p <*> parseMany p <|> pure []

parseSome :: Parser a -> Parser [a]
parseSome p = (:) <$> p <*> parseMany p

parseUInt :: Parser String
parseUInt = parseSome (parseAnyChar ['0'..'9'])

parseInt :: Parser String
parseInt =  (:) <$> parseChar '-' <*> (parseSome (parseAnyChar ['0'..'9'])) <|> parseUInt

parseDouble :: Parser Double
parseDouble = (\x -> read x :: Double) <$> ((\i c ui -> i ++ [c] ++ ui) <$> parseInt <*> parseChar '.' <*> parseUInt <|> parseInt)

parseTruple :: Parser Vector
parseTruple = (,,) <$> parseDouble <* parseChar ',' <*> parseDouble <* parseChar ',' <*> parseDouble

parseVector :: Parser Vector
parseVector = parseChar '(' *> parseTruple <* parseChar ')'


closestDistance :: Vector -> Vector -> Double -> [Vector] -> Vector
closestDistance _ close _ [] = close
closestDistance ref close dist (x:xs) | newDist < dist = closestDistance ref x newDist xs
                                    | otherwise = closestDistance ref close dist xs
                                    where newDist = distance ref x

main :: IO ()
main = do
    args <- getArgs
    case args of
        [a, b] -> do
            file <- readFile a
            let list = map fst $ mapMaybe (runParser parseVector) (lines file)
            case (runParser parseVector b, list) of
                (Just (vec,""), x:xs) -> print $ closestDistance vec x (distance vec x) xs
                (_, _) -> print "invalid vector or list is empty"
        _ -> return ()

-- rList :: String -> IO [Int]
-- rList = readIO