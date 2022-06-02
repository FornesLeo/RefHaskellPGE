
type Parser a = String -> Maybe (a , String)

parseChar :: Char -> Parser Char
parseChar x [] = Nothing
parseChar x (x':xs)
    | x == x'   = Just (x, xs)
    | otherwise = Nothing

parseAnyChar :: String -> Parser Char
parseAnyChar [] _ = Nothing
parseAnyChar _ [] = Nothing
parseAnyChar (x:xs) (y:ys) = case x == y of
                            True  -> Just (y, ys)
                            False -> parseAnyChar xs (y:ys)

parseAnyChar' :: String -> Parser Char
parseAnyChar' [] _ = Nothing
parseAnyChar' _ [] = Nothing
parseAnyChar' (x:xs) y = parseOr (parseChar x) (parseAnyChar' xs) y

parseOr :: Parser a -> Parser a -> Parser a
parseOr p p' str = case p str of
                Nothing      -> p' str
                Just (x, xs) -> Just (x, xs)

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd p p' str = case p str of
                Nothing      -> Nothing
                Just (x, xs) -> case p' xs of
                    Nothing      -> Nothing
                    Just (y, ys) -> Just ((x, y), ys)

parseAndWith ::  (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith f p p' str = case parseAnd p p' str of
                        Nothing           -> Nothing
                        Just ((x, y), ys) -> Just (f x y , ys)

parseMany :: Parser a -> Parser [a]
parseMany p str = case p str of 
                Nothing      -> Just ([], str)
                Just (x, xs) -> case parseMany p xs of 
                            Nothing      -> Just ([x], xs)
                            Just (y, ys) -> Just ((x:y), ys)

parseSome :: Parser a -> Parser [a]
parseSome p str = case p str of 
                Nothing      -> Nothing
                Just (x, xs) -> case parseMany p xs of 
                            Nothing      -> Just ([x], xs)
                            Just (y, ys) -> Just ((x:y), ys)

parseUInt :: Parser Int 
parseUInt str = case parseSome (parseAnyChar ['0'..'9']) str of
                Nothing      -> Nothing
                Just (x, xs) -> Just (read x :: Int, xs)

parseInt :: Parser Int 
parseInt str = case parseChar '-' str of
            Nothing      -> parseUInt str
            Just (x, xs) ->  case parseSome (parseAnyChar ['0'..'9']) xs of
                            Nothing      -> Nothing
                            Just (y, ys) -> Just (-(read y :: Int), ys)

parseTuple :: Parser a ->  Parser (a, a)
parseTuple p str = case parseAndWith (\ x y -> y) (parseChar '(') p str of
                Nothing       -> Nothing
                Just (x, xs) -> case parseAndWith (\ x y -> y) (parseChar ',') p xs of
                                Nothing      -> Nothing
                                Just (y, ys) -> case parseChar ')' ys of
                                            Nothing -> Nothing
                                            Just (_, zs) -> Just ((x, y), zs)