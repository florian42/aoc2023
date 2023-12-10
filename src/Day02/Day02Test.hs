module Main where

import Data.Char (isDigit)
import Data.List (findIndex)
import Day02 (Configuration (..), Cube (..), Game (..), HandOfCubes (..), Subset (..), gameParser, isGameValid, part1)
import Test.Hspec
import Text.Parsec (parse)

main :: IO ()
main = hspec $ do
  describe "gameParser" $ do
    it "extracts sample game 1" $ do
      let result = parse gameParser "" "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
      case result of
        Right game ->
          game
            `shouldBe` Game
              { gameID = 1,
                subsets =
                  [ Subset
                      { blue = 3,
                        green = 0,
                        red = 4
                      },
                    Subset
                      { blue = 6,
                        green = 2,
                        red = 1
                      },
                    Subset
                      { blue = 0,
                        green = 2,
                        red = 0
                      }
                  ]
              }
        Left error -> expectationFailure $ "Parsing failed: " ++ show error
    it "extracts game 5 with id 11" $ do
      let result = parse gameParser "" "Game 11: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
      case result of
        Right game ->
          game
            `shouldBe` Game
              { gameID = 11,
                subsets =
                  [ Subset
                      { blue = 1,
                        green = 3,
                        red = 6
                      },
                    Subset
                      { blue = 2,
                        green = 2,
                        red = 1
                      }
                  ]
              }
        Left error -> expectationFailure $ "Parsing failed: " ++ show error

  describe "isGameValid" $ do
    it "returns true for sample game 1" $ do
      let game =
            Game
              { gameID = 1,
                subsets =
                  [ Subset
                      { blue = 3,
                        green = 0,
                        red = 4
                      },
                    Subset
                      { blue = 6,
                        green = 2,
                        red = 1
                      },
                    Subset
                      { blue = 0,
                        green = 2,
                        red = 0
                      }
                  ]
              }
          config =
            Configuration
              { blueC = 14,
                greenC = 13,
                redC = 12
              }
      isGameValid game config `shouldBe` True
    it "returns true for sample game 2" $ do
      let game =
            Game
              { gameID = 1,
                subsets =
                  [ Subset
                      { blue = 1,
                        green = 2,
                        red = 0
                      },
                    Subset
                      { blue = 4,
                        green = 3,
                        red = 1
                      },
                    Subset
                      { blue = 1,
                        green = 1,
                        red = 0
                      }
                  ]
              }
          config =
            Configuration
              { blueC = 14,
                greenC = 13,
                redC = 12
              }
      isGameValid game config `shouldBe` True
    it "returns false for game 3" $ do
      let game =
            Game
              { gameID = 1,
                subsets =
                  [ Subset
                      { blue = 6,
                        green = 8,
                        red = 20
                      },
                    Subset
                      { blue = 5,
                        green = 13,
                        red = 4
                      },
                    Subset
                      { blue = 0,
                        green = 5,
                        red = 1
                      }
                  ]
              }
          config =
            Configuration
              { blueC = 14,
                greenC = 13,
                redC = 12
              }
      isGameValid game config `shouldBe` False
    it "returns false for game 4" $ do
      let game =
            Game
              { gameID = 1,
                subsets =
                  [ Subset
                      { blue = 6,
                        green = 1,
                        red = 3
                      },
                    Subset
                      { blue = 0,
                        green = 3,
                        red = 6
                      },
                    Subset
                      { blue = 15,
                        green = 3,
                        red = 14
                      }
                  ]
              }
          config =
            Configuration
              { blueC = 14,
                greenC = 13,
                redC = 12
              }
      isGameValid game config `shouldBe` False
    it "returns true for game 5" $ do
      let game =
            Game
              { gameID = 1,
                subsets =
                  [ Subset
                      { blue = 1,
                        green = 3,
                        red = 6
                      },
                    Subset
                      { blue = 2,
                        green = 2,
                        red = 1
                      }
                  ]
              }
          config =
            Configuration
              { blueC = 14,
                greenC = 13,
                redC = 12
              }
      isGameValid game config `shouldBe` True
  inputPart1sample <- runIO $ readFile "src/Day02/day02_sample_input.txt"
  realInput <- runIO $ readFile "src/Day02/day02_input.txt"
  describe "part1" $ do
    it "returns expected result on sample" $ do
      part1 inputPart1sample `shouldBe` 8
    it "returns expected result on real input" $ do
      part1 realInput `shouldBe` 8
