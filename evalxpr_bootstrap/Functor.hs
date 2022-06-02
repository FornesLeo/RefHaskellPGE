data Parser a = Parser {
    runParser :: String -> Maybe (a , String )
}

instance Functor Parser where
    fmap fct parser = Parser $ \s -> case runParser parser s of
                    Nothing      -> Nothing
                    Just (x, xs) -> Just (fct x, xs)

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
parseMany p = Parser $ \s -> case runParser p s of 
                Nothing      -> Just ([], s)
                Just (x, xs) -> case runParser (parseMany p) xs of 
                            Nothing      -> Just ([x], xs)
                            Just (y, ys) -> Just ((x:y), ys)

parseSome :: Parser a -> Parser [a]
parseSome p = Parser $ \s -> case runParser p s of 
                Nothing      -> Nothing
                Just (x, xs) -> case runParser (parseMany p) xs of 
                            Nothing      -> Just ([x], xs)
                            Just (y, ys) -> Just ((x:y), ys)

parseUInt :: Parser Int 
parseUInt =  fmap (\x -> read x :: Int) (parseSome (parseAnyChar ['0'..'9']))

parseUInt' :: Parser Int 
parseUInt' = (\x -> read x :: Int) <$> (parseSome (parseAnyChar ['0'..'9']))
