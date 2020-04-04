module StringCalculatorSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import StringCalculator

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "StringCalculator should able to calculate" $ do
    it "when input is `1`" $
      add "1" `shouldBe` Just 1
    it "when input is `1234567890`" $
      add "1234567890" `shouldBe` Just 1234567890
  describe "StringCalculator should not able to calculate" $ do
    it "when input is `ddd`" $
      add "ddd" `shouldBe` Nothing
    it "when input is `123x`" $
      add "123x" `shouldBe` Nothing

