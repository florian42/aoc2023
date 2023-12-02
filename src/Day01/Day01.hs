module Day01 where

import Data.Char (digitToInt, isDigit)
import Data.List (isPrefixOf)

extractNumber :: String -> (String -> String) -> Int
extractNumber input processInput = case length numbers of
  0 -> 0
  1 -> read [firstDigit, firstDigit] 
  _ -> read $ head numbers : [last numbers]
  where
    numbers = processInput input
    firstDigit = head numbers
    
extractNumbers :: String -> (String -> String) -> [Int]
extractNumbers input processInput = map (`extractNumber` processInput) (lines input)

part1 :: String -> Int
part1 input = sum $ extractNumbers input (filter isDigit)

digits = [("one", "1"), ("two", "2"), ("three", "3"), ("four", "4"), ("five", "5"), ("six", "6"), ("seven", "7"), ("eight", "8"), ("nine", "9")]

findValueForPrefix :: String -> Maybe (String, String)
findValueForPrefix str = case matches of
  (x : _) -> Just x
  [] -> Nothing
  where
    matches = filter (\(key, value) -> key `isPrefixOf` str) digits

processLetterDigits :: String -> String
processLetterDigits [] = []
processLetterDigits (x : xs)
  | isDigit x = x : processLetterDigits xs
  | otherwise = case findValueForPrefix (x : xs) of
      Just (matched, num) -> num ++ processLetterDigits xs
      Nothing -> processLetterDigits xs

part2 :: String -> Int
part2 input = sum $ extractNumbers input processLetterDigits
