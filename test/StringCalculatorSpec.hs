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
    it "when input is empty" $
      add "" `shouldBe` Just 0
    it "when input is `1`" $
      add "1" `shouldBe` Just 1
    it "when input is `123,456,789,0`" $
      add "123,456,789,0" `shouldBe` Just 1368
    it "when input is `1,2,3`" $
      add "1,2,3" `shouldBe` Just 6
    it "when input is `10,20,30,40,50`" $
      add "10,20,30,40,50" `shouldBe` Just 150
    it "when input is `1\n2,3`" $
      add "1\n2,3" `shouldBe` Just 6
    it "when input is `6,0\n10`" $
      add "6,0\n10" `shouldBe` Just 16
    it "when input when there are not new delimiters" $
      add "//\n1,2" `shouldBe` Just 3
    it "when input include one new delimiter" $
      add "//;\n1;2" `shouldBe` Just 3
    it "when number equal to 1000" $
      add "2,1000" `shouldBe` Just 1002
    it "when number bigger that 1000 and these ones should be ignored" $
      add "2,1001,1002" `shouldBe` Just 2
    it "when input include multiple new delimiter" $
      add "//[;][.]\n2;0.3.10" `shouldBe` Just 15
  describe "StringCalculator should not able to calculate" $ do
    it "when input is `ddd`" $
      add "ddd" `shouldBe` Nothing
    it "when input is `123x`" $
      add "123x" `shouldBe` Nothing
    it "when input is `,1,2,3`" $
      add ",1,2,3" `shouldBe` Nothing
    it "when input is `1,\n`" $
      add "1,\n" `shouldBe` Nothing
    it "when input is `/\n1`" $
      add "/\n1" `shouldBe` Nothing
    it "when input is `//;[]\n1;2[3]5`" $
      add "//;[]\n1;2[3]5" `shouldBe` Nothing
    it "when input is `-100`" $
      add "-100" `shouldBe` Nothing -- FIXME change to proper message `negatives not allowed`

