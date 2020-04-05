module StringCalculator (add) where 

import           Parser
import           Control.Applicative

add :: String -> Maybe Int
add = (>>= calculate) . runParser stringCalculatorParser
  where
    calculate(x,y) = if null x then Just $ sum $ filter(<=1000) y else Nothing

stringCalculatorParser :: Parser [Int]
stringCalculatorParser = [] <$ emptyInputP <|>
                          intsP defaultSepP <|>
                          andThenP delimitersP (intsP . newSepP)

defaultSepP :: Parser Char
defaultSepP = commaP <|> endOfLineP

newSepP :: String -> Parser Char
newSepP sep = oneOfP sep defaultSepP charP

delimitersP :: Parser String
delimitersP =  slashP *> slashP *> 
                  (
                    [] <$ endOfLineP <|>
                    (:[]) <$> ifP (const True) <* endOfLineP <|>
                    many(charP '[' *> ifP (const True) <* charP ']') <* endOfLineP
                  )

intsP :: Parser Char -> Parser [Int]
intsP sep = (:) <$> intP <*> sepBy sep intP
