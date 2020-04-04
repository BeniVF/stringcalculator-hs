module Main where

import StringCalculator

main :: IO ()
main = interact $ maybe "There was a problem" show . add
