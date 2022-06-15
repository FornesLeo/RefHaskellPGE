module Parser where

import Type

import Control.Applicative 

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
parseMany p = (:) <$> p <*> (parseMany p) <|> pure []

parseSome :: Parser a -> Parser [a]
parseSome p = (:) <$> p <*> (parseMany p)

parseUInt :: Parser String 
parseUInt = (parseSome (parseAnyChar ['0'..'9']))

parseInt :: Parser String
parseInt =  ((:) <$> (parseChar '-') <*> (parseSome (parseAnyChar ['0'..'9']))) <|> parseUInt

parseDouble :: Parser Double
parseDouble = (\x -> read x :: Double) <$> (((\i c ui -> i ++ [c] ++ ui) <$> parseInt <*> (parseChar '.') <*> parseUInt) <|> parseInt)

parseLit :: Parser Expr
parseLit = Lit <$> parseDouble

parsePar :: Parser Expr
parsePar = (Par <$> (parseChar '(' *> parseAdd <* parseChar ')')) <|> parseLit

parsePow :: Parser Expr
parsePow = (Pow <$> parsePar <* parseChar '^' <*> parsePar) <|> parsePar

parseDiv :: Parser Expr
parseDiv = (Div <$> parsePow <* parseChar '/' <*> parsePow) <|> parsePow

parseMul :: Parser Expr
parseMul = (Mul <$> parseDiv <* parseChar '*' <*> parseDiv) <|> parseDiv

parseSub :: Parser Expr
parseSub = (Sub <$> parseMul <* parseChar '-' <*> parseMul) <|> parseMul

parseAdd :: Parser Expr
parseAdd = (Add <$> parseSub <* parseChar '+' <*> parseAdd) <|> parseSub

parseExpr :: Parser Expr
parseExpr = parseAdd <|> parseLit