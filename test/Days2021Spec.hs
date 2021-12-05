{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Days2021Spec (spec) where

import qualified Days2021.Day1 as D1
import Days2021.Day2 (Command (..))
import qualified Days2021.Day2 as D2
import Days2021.Day3 (Bit (..))
import qualified Days2021.Day3 as D3
import Import
import Test.Hspec

spec :: Spec
spec = do
  describe "AoC 2021" $ do
    describe "exercise 1.1" $ do
      describe "countIncreases" $ do
        it "[199, 200, 208, 210, 200, 207, 240, 269, 260, 263] -> 7" $
          D1.countIncreases [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]
            `shouldBe` 7
    describe "exercise 1.2" $ do
      describe "generateWindows" $ do
        it "[199, 200, 208, 210, 200, 207, 240, 269] -> [(199, 200, 208) ... (269, 0, 0)]" $
          D1.generateWindows [199, 200, 208, 210, 200, 207, 240, 269]
            `shouldBe` [ (199, 200, 208),
                         (200, 208, 210),
                         (208, 210, 200),
                         (210, 200, 207),
                         (200, 207, 240),
                         (207, 240, 269),
                         (240, 269, 0),
                         (269, 0, 0)
                       ]
      describe "countWindowIncreases" $ do
        it "[199, 200, 208, 210, 200, 207, 240, 269, 260, 263] -> 5" $ do
          D1.countWindowIncreases [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]
            `shouldBe` 5
    describe "exercise 2.1" $ do
      describe "move" $ do
        it "(0, 0) -> [] -> (0, 0)" $ do
          D2.stepMove (0, 0) [] `shouldBe` (0, 0)
        it "(0, 0) -> [F5, D5, F8, U3, D8, F2] -> (15, 10)" $ do
          D2.stepMove (0, 0) [F 5, D 5, F 8, U 3, D 8, F 2] `shouldBe` (15, 10)
    describe "exercise 2.2" $ do
      describe "bearingMove" $ do
        it "(0, (0, 0)) -> [] -> (0, (0, 0))" $ do
          D2.bearingMove (0, (0, 0)) [] `shouldBe` (0, (0, 0))
        it "(0, (0, 0)) -> [F5, D5, F8, U3, D8, F2] -> (10, (15, 60))" $ do
          D2.bearingMove (0, (0, 0)) [F 5, D 5, F 8, U 3, D 8, F 2] `shouldBe` (10, (15, 60))
    describe "exercise 3.1" $ do
      describe "getMostCommon" $ do
        it "[0,0,1] -> 0" $ do
          D3.getMostCommon [Zero, Zero, One] `shouldBe` Zero
        it "[0,1,1] -> 0" $ do
          D3.getMostCommon [Zero, One, One] `shouldBe` One
        it "[0,0,1,1] -> 0" $ do
          D3.getMostCommon [Zero, Zero, One, One] `shouldBe` One
      describe "getLeastCommon" $ do
        it "[0,0,1] -> 0" $ do
          D3.getLeastCommon [Zero, Zero, One] `shouldBe` One
        it "[0,1,1] -> 0" $ do
          D3.getLeastCommon [Zero, One, One] `shouldBe` Zero
        it "[0,0,1,1] -> 0" $ do
          D3.getLeastCommon [Zero, Zero, One, One] `shouldBe` Zero
      describe "binNumToInt" $ do
        it "10110 -> 22" $ do
          let binNum = D3.parseBinNum "10110"
          D3.binNumToInt binNum `shouldBe` 22
      describe "getGammaRate" $ do
        it "00100, 11110, 10110, 10111, 10101, 01111, 00111, 11100, 10000, 11001, 00010, 01010 -> 22" $ do
          let bitNumStrings = ["00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010"]
          let bitNums = D3.parseBinNum <$> bitNumStrings
          (D3.binNumToInt . D3.getGammaRate) bitNums `shouldBe` 22
      describe "getEpsilonRate" $ do
        it "00100, 11110, 10110, 10111, 10101, 01111, 00111, 11100, 10000, 11001, 00010, 01010 -> 9" $ do
          let bitNumStrings = ["00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010"]
          let bitNums = D3.parseBinNum <$> bitNumStrings
          (D3.binNumToInt . D3.getEpsilonRate) bitNums `shouldBe` 9
      describe "filterByMostCommon" $ do
        it "00100, 11110, 10110, 10111, 10101, 01111, 00111, 11100, 10000, 11001, 00010, 01010 -> 23" $ do
          let bitNumStrings = ["00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010"]
          let bitNums = D3.parseBinNum <$> bitNumStrings
          D3.binNumToInt <$> D3.filterByMostCommon 0 bitNums `shouldBe` [23]
      describe "filterByLeastCommon" $ do
        it "00100, 11110, 10110, 10111, 10101, 01111, 00111, 11100, 10000, 11001, 00010, 01010 -> 10" $ do
          let bitNumStrings = ["00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010"]
          let bitNums = D3.parseBinNum <$> bitNumStrings
          D3.binNumToInt <$> D3.filterByLeastCommon 0 bitNums `shouldBe` [10]