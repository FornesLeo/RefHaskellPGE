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
    (*>) p1 p2 = (flip const) <$> p1 <*> p2

instance Alternative Parser where
    p1 <|> p2 = Parser $ \s -> case runParser p1 s of
        Nothing -> runParser p2 s
        Just (r, s') -> Just (r, s')

    empty = Parser $ \s -> Nothing

instance Monad Parser where
    (>>=) p f = Parser $ \s -> case runParser p s of
        Nothing -> Nothing
        Just (r, s') -> runParser (f r) s'

