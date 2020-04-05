module Parser where

import           Data.Char
import           Data.Bifunctor
import           Data.Tuple
import           Control.Applicative

newtype Parser a = Parser { runParser :: String -> Maybe (String, a) }

instance Functor Parser where
  fmap f (Parser p) = Parser $ fmap(second f) . p

instance Applicative Parser where
  pure x = Parser (\y -> Just (y, x))
  (Parser p1) <*> (Parser p2) = Parser $ \x -> do 
                                    (i1, f) <- p1 x
                                    (i2, a) <- p2 i1
                                    return (i2, f a)

instance Alternative Parser where
  empty = Parser (const Nothing)
  (Parser p1) <|> (Parser p2) = Parser $
    \x -> p1 x <|> p2 x

andThenP :: Parser a -> (a -> Parser [b]) -> Parser [b]
andThenP p f = Parser $ \x ->
    do 
      (r, s) <- runParser p x
      runParser (f s) r

slashP :: Parser Char
slashP = charP '/'

emptyInputP :: Parser ()
emptyInputP = Parser $ \x -> if null x then Just("", ()) else Nothing

intP :: Parser Int
intP = (\x y -> read $ x : y) <$> digitP <*> spanP isDigit

digitP :: Parser Char
digitP = ifP isDigit

spanP :: (Char -> Bool) -> Parser String
spanP = many . ifP

ifP :: (Char -> Bool) -> Parser Char
ifP g = Parser f 
  where 
    f (x:xs) | g x = Just (xs, x)
             | otherwise = Nothing
    f _ = Nothing

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy p1 p2 = many $ p1 *> p2

commaP :: Parser Char
commaP = charP ','

endOfLineP :: Parser Char
endOfLineP = charP '\n'

charP:: Char -> Parser Char
charP c = Parser f
  where
    f (x:xs) | x == c = Just (xs, x)
             | otherwise = Nothing
    f _ = Nothing

oneOfP :: [a] -> Parser a -> (a -> Parser a) -> Parser a
oneOfP (x:xs) d f = f x <|> oneOfP xs d f
oneOfP _ d _ = d