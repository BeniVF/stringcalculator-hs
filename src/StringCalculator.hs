module StringCalculator (add) where 

import           Parser
import           Control.Applicative

add :: String -> Maybe Int
add = (>>= calculate) . runParser stringCalculatorParser
  where
    calculate(x,y) = if null x then Just $ sum $ filter(<=1000) y else Nothing
