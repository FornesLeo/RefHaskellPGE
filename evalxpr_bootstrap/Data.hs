data Parser a = Parser {
    runParser :: String -> Maybe (a , String )
}

parsePred :: (Char -> Bool) -> Parser Char
parsePred f = Parser $ \ s -> case s of
  [] -> Nothing
  (x:xs) | f x -> Just (x, xs)
         | otherwise -> Nothing

parseChar :: Char -> Parser Char
parseChar x = parsePred (==x)

parseAnyChar :: String -> Parser Char
parseAnyChar x = parsePred (`elem` x)

parseOr :: Parser a -> Parser a -> Parser a
parseOr p p' = Parser $ \s -> case runParser p s of
                      Just (x, xs) -> Just (x, xs)
                      Nothing      -> runParser p' s

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd p p' = Parser $ \s -> case runParser p s of
                      Nothing      -> Nothing
                      Just (x, xs) -> case runParser p' xs of
                              Nothing      -> Nothing
                              Just (y, ys) -> Just ((x, y), ys)
                    

parseAndWith ::  (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith f p p' = Parser $ \s -> case runParser (parseAnd p p') s of
                        Nothing           -> Nothing
                        Just ((x, y), ys) -> Just (f x y , ys)

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
parseUInt =  Parser $ \s -> case runParser (parseSome (parseAnyChar ['0'..'9'])) s of
                Nothing      -> Nothing
                Just (x, xs) -> Just (read x :: Int, xs)

parseInt :: Parser Int 
parseInt =  Parser $ \s -> case runParser (parseChar '-') s of
            Nothing      -> runParser parseUInt s
            Just (x, xs) ->  case runParser (parseSome (parseAnyChar ['0'..'9'])) xs of
                            Nothing      -> Nothing
                            Just (y, ys) -> Just (-(read y :: Int), ys)

parseTuple :: Parser a ->  Parser (a, a)
parseTuple p = Parser $ \s -> case runParser (parseAndWith (\ x y -> y) (parseChar '(') p) s of
                Nothing       -> Nothing
                Just (x, xs) -> case runParser (parseAndWith (\ x y -> y) (parseChar ',') p) xs of
                                Nothing      -> Nothing
                                Just (y, ys) -> case runParser (parseChar ')') ys of
                                            Nothing -> Nothing
                                            Just (_, zs) -> Just ((x, y), zs)