module Main where

import Data.Char (isDigit)
import Data.List (findIndex)
import Day02 (Game (..), HandOfCubes (..), gameParser, Cube (..), countCubes, isGameValid, Configuration (..))
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
  
  describe "isGameValid" $ do
    it "returns true for sample game 1" $ do
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
          config = Configuration
                      { blue = 14,
                        green = 13,
                        red = 12
                      }
      isGameValid game config `shouldBe` True
    it "returns true for sample game 2" $ do
      let game = Game
                  { gameID = 1,
                    hand =
                      [ HandOfCubes {count = 1, cube = Blue},
                        HandOfCubes {count = 2, cube = Green},
                        HandOfCubes {count = 3, cube = Green},
                        HandOfCubes {count = 4, cube = Blue},
                        HandOfCubes {count = 1, cube = Red},
                        HandOfCubes {count = 1, cube = Green},
                        HandOfCubes {count = 1, cube = Blue}
                      ]
                  }
          config = Configuration
                      { blue = 14,
                        green = 13,
                        red = 12
                      }
      isGameValid game config `shouldBe` True
    it "returns false for game 3" $ do
      let game = Game
                  { gameID = 1,
                    hand =
                      [ HandOfCubes {count = 8, cube = Green},
                        HandOfCubes {count = 6, cube = Blue},
                        HandOfCubes {count = 20, cube = Red},
                        HandOfCubes {count = 5, cube = Blue},
                        HandOfCubes {count = 4, cube = Red},
                        HandOfCubes {count = 13, cube = Green},
                        HandOfCubes {count = 5, cube = Green},
                        HandOfCubes {count = 1, cube = Red}
                      ]
                  }
          config = Configuration
                      { blue = 14,
                        green = 13,
                        red = 12
                      }
      isGameValid game config `shouldBe` False
    it "returns false for game 4" $ do
      let game = Game
                  { gameID = 1,
                    hand =
                      [ HandOfCubes {count = 1, cube = Green},
                        HandOfCubes {count = 3, cube = Red},
                        HandOfCubes {count = 6, cube = Blue},
                        HandOfCubes {count = 3, cube = Green},
                        HandOfCubes {count = 6, cube = Red},
                        HandOfCubes {count = 3, cube = Green},
                        HandOfCubes {count = 15, cube = Blue},
                        HandOfCubes {count = 14, cube = Red}
                      ]
                  }
          config = Configuration
                      { blue = 14,
                        green = 13,
                        red = 12
                      }
      isGameValid game config `shouldBe` False
    it "returns true for game 5" $ do
      let game = Game
                  { gameID = 1,
                    hand =
                      [ HandOfCubes {count = 6, cube = Red},
                        HandOfCubes {count = 1, cube = Blue},
                        HandOfCubes {count = 3, cube = Green},
                        HandOfCubes {count = 2, cube = Blue},
                        HandOfCubes {count = 1, cube = Red},
                        HandOfCubes {count = 2, cube = Green}
                      ]
                  }
          config = Configuration
                      { blue = 14,
                        green = 13,
                        red = 12
                      }
      isGameValid game config `shouldBe` True


