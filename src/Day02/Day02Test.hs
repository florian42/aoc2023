module Main where

import Data.Char (isDigit)
import Data.List (findIndex)
import Day02 (Game (..), HandOfCubes (..), gameParser, Cube (..), countCubes)
import Test.Hspec
import Text.Parsec (parse)

main :: IO ()
main = hspec $ do
  describe "gameParser" $ do
    it "extracts sample game 1" $ do
      let result = parse gameParser "" "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
      case result of
        Right game -> game `shouldBe` Game
                        { gameID = 1,
                          hand =
                            [ HandOfCubes {count = 3, cube = Blue},
                              HandOfCubes {count = 4, cube = Red},
                              HandOfCubes {count = 1, cube = Red},
                              HandOfCubes {count = 2, cube = Green},
                              HandOfCubes {count = 6, cube = Blue},
                              HandOfCubes {count = 2, cube = Green}
                            ]
                        }
        Left error -> expectationFailure $ "Parsing failed: " ++ show error
    it "extracts game 5 with id 11" $ do
      let result = parse gameParser "" "Game 11: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
      case result of
        Right game -> game `shouldBe` Game
                        { gameID = 11,
                          hand =
                            [ HandOfCubes {count = 6, cube = Red},
                              HandOfCubes {count = 1, cube = Blue},
                              HandOfCubes {count = 3, cube = Green},
                              HandOfCubes {count = 2, cube = Blue},
                              HandOfCubes {count = 1, cube = Red},
                              HandOfCubes {count = 2, cube = Green}
                            ]
                        }
        Left error -> expectationFailure $ "Parsing failed: " ++ show error
  describe "countCubes" $ do
    it "counts blue cubes" $ do
      let game = Game
                  { gameID = 1,
                    hand =
                      [ HandOfCubes {count = 3, cube = Blue},
                        HandOfCubes {count = 4, cube = Red},
                        HandOfCubes {count = 1, cube = Red},
                        HandOfCubes {count = 2, cube = Green},
                        HandOfCubes {count = 6, cube = Blue},
                        HandOfCubes {count = 2, cube = Green}
                      ]
                  }
      countCubes Blue (hand game) `shouldBe` 9
    it "counts red cubes" $ do
      let game = Game
                  { gameID = 1,
                    hand =
                      [ HandOfCubes {count = 3, cube = Blue},
                        HandOfCubes {count = 4, cube = Red},
                        HandOfCubes {count = 1, cube = Red},
                        HandOfCubes {count = 2, cube = Green},
                        HandOfCubes {count = 6, cube = Blue},
                        HandOfCubes {count = 2, cube = Green}
                      ]
                  }
      countCubes Red (hand game) `shouldBe` 5

