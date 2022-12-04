{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Days2022.Day1Spec (spec) where

import Days2022.Day1
  ( getMaxCalorieCount,
    parseInput,
  )
import Import
import Test.Hspec

spec :: Spec
spec = do
  describe "exercise 1" $ do
    describe "parseInput" $ do
      it "[] -> []" $ do
        let input = []
        parseInput input `shouldBe` []
      it "['1'] -> [[1]]" $ do
        let input = ["1"]
        parseInput input `shouldBe` [[1]]
      it "['1', ''] -> [[],[1]]" $ do
        let input = ["1", ""]
        parseInput input `shouldBe` [[], [1]]

      it "['123', '', '45'] -> [[],[1]]" $ do
        let input = ["1", "2", "3", "", "4", "5"]
        parseInput input `shouldBe` [[5, 4], [3, 2, 1]]

    describe "getMaxCalorieCount" $ do
      it "solves the example" $ do
        let calorieBatches =
              [ [ 1000,
                  2000,
                  3000
                ],
                [ 4000
                ],
                [ 5000,
                  6000
                ],
                [ 7000,
                  8000,
                  9000
                ],
                [ 10000
                ]
              ]
        getMaxCalorieCount calorieBatches `shouldBe` 24000