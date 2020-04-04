module StringCalculator (add) where
import           Data.Char
import           Data.Bifunctor
import           Data.Tuple
import           Control.Applicative

add :: String -> Maybe Int
add = (>>= calculate) . runParser stringCalculatorParser
  where
    calculate(x,y) = if null x then Just y else Nothing

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


stringCalculatorParser :: Parser Int
stringCalculatorParser = intP

intP :: Parser Int
intP = read <$> spanP isDigit

spanP :: (Char -> Bool) -> Parser String
spanP = many . ifP

ifP :: (Char -> Bool) -> Parser Char
ifP g = Parser f 
  where 
    f (x:xs) | g x = Just (xs, x)
             | otherwise = Nothing
    f _ = Nothing



