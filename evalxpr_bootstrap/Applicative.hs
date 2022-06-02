data Parser a = Parser {
    runParser :: String -> Maybe (a , String )
}

instance Functor Parser where
    fmap fct parser = Parser $ \s -> case runParser parser s of
                    Nothing      -> Nothing
                    Just (x, xs) -> Just (fct x, xs)

instance Applicative Parser where
    pure x     = Parser $ \s -> Just (x, s)
    (<*>) p p' = Parser $ \s -> case runParser p s of
                      Nothing      -> Nothing
                      Just (f, s') -> case runParser p' s' of
                              Nothing      -> Nothing
                              Just (x, xs) -> Just (f x, xs)
    (<*) p p' = const <$> p <*> p'
    (*>) p p' = (flip const) <$> p <*> p'

(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = Parser $ \s -> case runParser p1 s of
    Nothing -> runParser p2 s
    Just (x, xs) -> Just (x, xs)


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
parseOr p p' = p <|> p'

parseSome :: Parser a -> Parser [a]
parseSome p = (:) <$> p <*> (parseSome p) <|> pure []

parseUInt :: Parser Int 
parseUInt = (\x -> read x :: Int) <$> (parseSome (parseAnyChar ['0'..'9']))

parseInt :: Parser Int 
parseInt =  ((\x -> read x :: Int) <$> ((:) <$> (parseChar '-')<*> (parseSome (parseAnyChar ['0'..'9'])))) <|> parseUInt


