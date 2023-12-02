module Main where

import Data.Char (isDigit)
import Day01
import Test.Hspec

main :: IO ()
main = hspec $ do
  -- input <- runIO $ readFile "day01_input.txt"
  let filter' = filter isDigit
  describe "extractNumber" $ do
    it "extracts 1abc2 properly" $ do
      extractNumber "1abc2" filter' `shouldBe` 12
    it "extracts pqr3stu8vwx properly" $ do
      extractNumber "pqr3stu8vwx" filter' `shouldBe` 38
    it "extracts pqr3stu8vwx properly" $ do
      extractNumber "pqr3stu8vwx" filter' `shouldBe` 38
    it "extracts a1b2c3d4e5f properly" $ do
      extractNumber "a1b2c3d4e5f" filter' `shouldBe` 15
    it "extracts treb7uchet properly" $ do
      extractNumber "treb7uchet" filter' `shouldBe` 77

  input <- runIO $ readFile "src/Day01/day01_input.txt"
  describe "part1" $ do
    it "extracts testInputFile" $ do
      part1 input `shouldBe` 54877
