module Day01 where

import Data.Char (digitToInt, isDigit)

extractNumber :: String -> (String -> String) -> Int
extractNumber input processInput = case length numbers of
  1 -> read [firstDigit, firstDigit]
  _ -> read $ head numbers : [last numbers]
  where
    numbers = processInput input
    firstDigit = head numbers

extractNumbers :: String -> [Int]
extractNumbers input = map (\line -> extractNumber line (filter isDigit)) (lines input)

part1 :: String -> Int
part1 input = sum $ extractNumbers input
