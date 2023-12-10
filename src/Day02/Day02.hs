module Day02 where

import Text.Parsec
    ( digit, space, string, many1, sepBy, (<|>), try )
import Text.Parsec.String (Parser)
import Text.Parsec.Char ( digit, space, string )
import Text.Parsec.Combinator ( many1, sepBy )


type GameID = Int

data Cube = Blue | Green | Red deriving (Eq, Show)

data HandOfCubes = HandOfCubes
  { count :: Int,
    cube :: Cube
  } deriving (Eq, Show)

data Game = Game
  { gameID :: GameID,
    hand :: [HandOfCubes]
  } deriving (Eq, Show)

cubeParser :: Parser Cube
cubeParser =
  (string "blue" >> return Blue)
    <|> (string "red" >> return Red)
    <|> (string "green" >> return Green)

handOfCubesParser :: Parser HandOfCubes
handOfCubesParser = do
    count <- read <$> many1 digit
    space
    HandOfCubes count <$> cubeParser


gameParser :: Parser Game
gameParser = do
  string "Game "
  gameId <- read <$> many1 digit
  string ": "
  hands <- handOfCubesParser `sepBy` (try (string "; ") <|> string ", ")
  return $ Game gameId hands