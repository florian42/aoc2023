module Day02 where

import Text.Parsec
    ( digit, space, string, many1, sepBy, (<|>), try, parse )
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

data Configuration = Configuration
  {  blue :: Int,
     green :: Int,
     red :: Int
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

countCubes :: Cube -> [HandOfCubes] -> Int
countCubes targetCube cubes = sum $ map count $ filter (\hand -> targetCube == cube hand) cubes

isGameValid :: Game -> Configuration -> Bool
isGameValid game config = countCubes Blue (hand game)  <= blue config
                       && countCubes Green (hand game) <= green config
                       && countCubes Red (hand game)   <= red config

part1 :: String -> Int
part1 input = sum $ map gameID $ filter (`isGameValid` config ) parsedGames
  where
    config = Configuration { blue = 14, green = 13, red = 12}
    parsedGames = map (either (error . show) id . parse gameParser "") (lines input)