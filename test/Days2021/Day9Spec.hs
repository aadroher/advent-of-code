{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Days2021.Day9Spec (spec) where

import qualified Days2021.Day9 as D9
import Import
import qualified RIO.Set as S
import Test.Hspec

spec :: Spec
spec = do
  describe "exercise 9.1" $ do
    describe "isValid" $ do
      it "returns True for a valid map" $ do
        let floorMap =
              [ [0, 0],
                [0, 0]
              ]
        D9.isValid floorMap `shouldBe` True
      it "returns False for an invalid map" $ do
        let floorMap =
              [ [0, 0],
                [0, 0],
                [0, 0, 0]
              ]
        D9.isValid floorMap `shouldBe` False
    describe "dimensions" $ do
      it "returns dimensions for a valid map" $ do
        let floorMap =
              [ [0, 0],
                [0, 0],
                [0, 0]
              ]
        D9.dimensions floorMap `shouldBe` Just (2, 3)
      it "returns Nothing for an invalid map" $ do
        let floorMap =
              [ [0, 0],
                [0, 0],
                [0, 0, 0]
              ]
        D9.dimensions floorMap `shouldBe` Nothing
    describe "inBounds" $ do
      it "returns True for a valid position" $ do
        let floorMap =
              [ [0, 0],
                [0, 0],
                [0, 0]
              ]
        D9.inBounds (1, 2) floorMap `shouldBe` True
      it "returns False for an invalid position" $ do
        let floorMap =
              [ [0, 0],
                [0, 0],
                [0, 0]
              ]
        D9.inBounds (2, 1) floorMap `shouldBe` False
      it "returns False for an position with negative indexes" $ do
        let floorMap =
              [ [0, 0],
                [0, 0],
                [0, 0]
              ]
        D9.inBounds (2, -1) floorMap `shouldBe` False
    describe "valueAt" $ do
      it "valueAt Just value for a valid position" $ do
        let floorMap =
              [ [0, 0],
                [0, 0],
                [0, 5]
              ]
        D9.valueAt (1, 2) floorMap `shouldBe` Just 5
      it "returns Nothing for an invalid position" $ do
        let floorMap =
              [ [0, 0],
                [0, 0],
                [0, 0]
              ]
        D9.valueAt (2, 1) floorMap `shouldBe` Nothing
    describe "neighbours" $ do
      it "returns neighbour set for a valid position on a side" $ do
        let floorMap =
              [ [0, 0],
                [0, 0],
                [0, 0]
              ]
        D9.neighbours (1, 1) floorMap
          `shouldBe` S.fromList
            [ (0, 0),
              (1, 0),
              (0, 1),
              (0, 2),
              (1, 2)
            ]
      it "returns neighbour set for a valid position on a corner" $ do
        let floorMap =
              [ [0, 0],
                [0, 0],
                [0, 0]
              ]
        D9.neighbours (1, 2) floorMap
          `shouldBe` S.fromList
            [ (0, 2),
              (0, 1),
              (1, 1)
            ]
      it "returns neighbour set for an adjacent invalid position" $ do
        let floorMap =
              [ [0, 0],
                [0, 0],
                [0, 0]
              ]
        D9.neighbours (2, 1) floorMap
          `shouldBe` S.fromList
            [ (1, 0),
              (1, 1),
              (1, 2)
            ]
    describe "lowPoints" $ do
      it "solves the example" $ do
        let floorMap =
              [ [2, 1, 9, 9, 9, 4, 3, 2, 1, 0],
                [3, 9, 8, 7, 8, 9, 4, 9, 2, 1],
                [9, 8, 5, 6, 7, 8, 9, 8, 9, 2],
                [8, 7, 6, 7, 8, 9, 6, 7, 8, 9],
                [9, 8, 9, 9, 9, 6, 5, 6, 7, 8]
              ]
        D9.lowPoints floorMap
          `shouldBe` S.fromList
            [ (1, 0),
              (9, 0),
              (2, 2),
              (6, 4)
            ]
    describe "sumRiskLevels" $ do
      it "solves the example" $ do
        let floorMap =
              [ [2, 1, 9, 9, 9, 4, 3, 2, 1, 0],
                [3, 9, 8, 7, 8, 9, 4, 9, 2, 1],
                [9, 8, 5, 6, 7, 8, 9, 8, 9, 2],
                [8, 7, 6, 7, 8, 9, 6, 7, 8, 9],
                [9, 8, 9, 9, 9, 6, 5, 6, 7, 8]
              ]
        D9.sumRiskLevels floorMap `shouldBe` 15
    describe "parseRow" $ do
      it "parses row" $ do
        D9.parseRow "2199943210" `shouldBe` [2, 1, 9, 9, 9, 4, 3, 2, 1, 0]