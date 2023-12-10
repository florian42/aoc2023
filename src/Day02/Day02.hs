module Day02 where

import Text.Parsec
  ( digit,
    many1,
    parse,
    sepBy,
    space,
    string,
    try,
    (<|>),
  )
import Text.Parsec.Char (digit, space, string)
import Text.Parsec.Combinator (many1, sepBy)
import Text.Parsec.String (Parser)

type GameID = Int

data Cube = Blue | Green | Red deriving (Eq, Show)

data HandOfCubes = HandOfCubes
  { count :: Int,
    cube :: Cube
  }
  deriving (Eq, Show)

data Subset = Subset
  { blue :: Int,
    green :: Int,
    red :: Int
  }
  deriving (Eq, Show)

data Game = Game
  { gameID :: GameID,
    subsets :: [Subset]
  }
  deriving (Eq, Show)

data Configuration = Configuration
  { blueC :: Int,
    greenC :: Int,
    redC :: Int
  }
  deriving (Eq, Show)

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

subsetParser :: Parser Subset
subsetParser = do
  hands <- handOfCubesParser `sepBy` string ", "
  let blue = sum $ map count $ filter (\hand -> Blue == cube hand) hands
  let green = sum $ map count $ filter (\hand -> Green == cube hand) hands
  let red = sum $ map count $ filter (\hand -> Red == cube hand) hands
  return $ Subset blue green red

gameParser :: Parser Game
gameParser = do
  string "Game "
  gameId <- read <$> many1 digit
  string ": "
  subsets <- subsetParser `sepBy` string "; "
  return $ Game gameId subsets


isSubsetValid :: Subset -> Configuration -> Bool
isSubsetValid subset config =
  blue subset <= blueC config
    && green subset <= greenC config
    && red subset <= redC config

isGameValid :: Game -> Configuration -> Bool
isGameValid game config = all (`isSubsetValid` config) (subsets game)

part1 :: String -> Int
part1 input = sum $ map gameID $ filter (`isGameValid` config) parsedGames
  where
    config = Configuration {blueC = 14, greenC = 13, redC = 12}
    parsedGames = map (either (error . show) id . parse gameParser "") (lines input)