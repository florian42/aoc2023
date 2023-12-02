module Main where

import Data.Char (isDigit)
import Data.List (findIndex)
import Day01 (extractNumber, findValueForPrefix, part1, part2, processLetterDigits)
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

  describe "findValueForPrefix" $ do
    it "finds digit" $ do
      findValueForPrefix "one2three" `shouldBe` Just ("one", "1")
    it "returns nothing in no match" $ do
      findValueForPrefix "1one2" `shouldBe` Nothing

  describe "processLetterDigits" $ do
    it "converts spelled-out digits" $ do
      processLetterDigits "onetwothree" `shouldBe` "123"
    it "handles mix of spelled-out and numeric digits" $ do
      processLetterDigits "one2three4" `shouldBe` "1234"
    it "processes digits with interspersed non-digits" $ do
      processLetterDigits "a1b2cthree4d" `shouldBe` "1234"
    it "handles spelled-out digits at start and end" $ do
      processLetterDigits "onexyztwo" `shouldBe` "12"
    it "handles strings with no digits" $ do
      processLetterDigits "abcdefg" `shouldBe` ""
    it "handles single digit" $ do
      processLetterDigits "abc4xyz" `shouldBe` "4"
    it "converts all spelled-out digits in sequence" $ do
      processLetterDigits "onetwothreefourfivesixseveneightnine" `shouldBe` "123456789"
    it "handles complex mix of characters" $ do
      processLetterDigits "a1b!two@3#four$" `shouldBe` "1234"
    it "handles mixed digits and spelled-out digits without separation" $ do
      processLetterDigits "four2" `shouldBe` "42"
    it "does not mistake partial matches of spelled-out digits" $ do
      processLetterDigits "onex" `shouldBe` "1" -- ??
    it "handles consecutive spelled-out digits" $ do
      processLetterDigits "twothree" `shouldBe` "23"
    it "ignores non-digit characters" $ do
      processLetterDigits "!@#one$%^&*()" `shouldBe` "1"
    it "handles long strings with no valid digits" $ do
      processLetterDigits "abcdefghijk" `shouldBe` ""
    it "handles twone" $ do
      processLetterDigits "twone" `shouldBe` "21"
    -- it "handles case sensitivity" $ do
    --  processLetterDigits "OneTwoThree" `shouldBe` "123" -- ??

  inputPart2sample <- runIO $ readFile "src/Day01/day01_sample_input_2.txt"
  describe "part2" $ do
    it "extracts testInputFile" $ do
      part2 inputPart2sample `shouldBe` 281
    it "extracts hlpqrdh3" $ do
      part2 "hlpqrdh3" `shouldBe` 33
    it "extracts real input" $ do
      part2 input `shouldBe` 281
    it "calculates the correct sum for basic spelled-out and numeric digits" $ do
      part2 "one2\nthree4" `shouldBe` (12 + 34)
    it "handles lines with a single digit correctly" $ do
      part2 "five\n9" `shouldBe` (55 + 99)
    it "handles mixed characters and digits" $ do
      part2 "a1b2c\nxseveny" `shouldBe` (12 + 77)
    it "handles lines with no valid digits" $ do
      part2 "abc\ndef" `shouldBe` 0
    it "handles consecutive spelled-out digits" $ do
      part2 "twothree\nfourfive" `shouldBe` (23 + 45)
    it "handles complex scenarios with various patterns" $ do
      part2 "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet" `shouldBe` (12 + 38 + 15 + 77)
    it "handles edge cases and special characters" $ do
      part2 "!@#two$%^&*\none3four" `shouldBe` (22 + 14)
    it "long excerpt" $ do
      part2 "four82nine74\nhlpqrdh3\neightsevenhrsseven988\n324pzonenine\nfglpbone79fourvrgcmgklbmthree\nfmbbkvthdcdmcjxzclk42six4\nfour22xcqsnvktnpfshtmm\nqmfsccxsixfivelnmpjqjcsc1sixpfpmeight\neight1nine5nine9six\ns4r91seven" `shouldBe` (44+33+88+39+13+44+42+68+86+47)

