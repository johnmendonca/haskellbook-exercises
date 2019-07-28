module WordNumber where

import Data.List (intersperse)
import Data.Char (digitToInt)

import Test.Hspec

digitWords :: [String]
digitWords = ["zero","one","two","three","four","five","six","seven","eight","nine"]

digitToWord :: Int -> String
digitToWord n = digitWords !! n

digits :: Int -> [Int]
digits n = map digitToInt $ show n

wordNumber :: Int -> String
wordNumber n = concat $ intersperse "-" $ map digitToWord $ digits n

main :: IO ()
main = hspec $ do
  describe "digitToWord does what we want" $ do
    it "returns zero for 0" $ do
      digitToWord 0 `shouldBe` "zero"
    it "returns one for 1" $ do
      digitToWord 1 `shouldBe` "one"

  describe "digits does what we want" $ do
    it "returns [1] for 1" $ do
      digits 1 `shouldBe` [1]
    it "returns [1,0,0] for 100" $ do
      digits 100 `shouldBe` [1,0,0]

  describe "wordNumber does what we want" $ do
    it "returns one-zero-zero for 100" $ do
      wordNumber 100 `shouldBe` "one-zero-zero"
    it "returns nine-zero-zero-one for 9001" $ do
      wordNumber 9001 `shouldBe` "nine-zero-zero-one"

