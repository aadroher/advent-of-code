{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Days2021.Day3Spec (spec) where

import Days2021.Day3
  ( Bit (One, Zero),
    binNumToInt,
    filterByLeastCommon,
    filterByMostCommon,
    getEpsilonRate,
    getGammaRate,
    getLeastCommon,
    getMostCommon,
    parseBinNum,
  )
import Import
import Test.Hspec

spec :: Spec
spec = do
  describe "exercise 3.1" $ do
    describe "getMostCommon" $ do
      it "[0,0,1] -> 0" $ do
        getMostCommon [Zero, Zero, One] `shouldBe` Zero
      it "[0,1,1] -> 0" $ do
        getMostCommon [Zero, One, One] `shouldBe` One
      it "[0,0,1,1] -> 0" $ do
        getMostCommon [Zero, Zero, One, One] `shouldBe` One
    describe "getLeastCommon" $ do
      it "[0,0,1] -> 0" $ do
        getLeastCommon [Zero, Zero, One] `shouldBe` One
      it "[0,1,1] -> 0" $ do
        getLeastCommon [Zero, One, One] `shouldBe` Zero
      it "[0,0,1,1] -> 0" $ do
        getLeastCommon [Zero, Zero, One, One] `shouldBe` Zero
    describe "binNumToInt" $ do
      it "10110 -> 22" $ do
        let binNum = parseBinNum "10110"
        binNumToInt binNum `shouldBe` 22
    describe "getGammaRate" $ do
      it "00100, 11110, 10110, 10111, 10101, 01111, 00111, 11100, 10000, 11001, 00010, 01010 -> 22" $ do
        let bitNumStrings = ["00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010"]
        let bitNums = parseBinNum <$> bitNumStrings
        (binNumToInt . getGammaRate) bitNums `shouldBe` 22
    describe "getEpsilonRate" $ do
      it "00100, 11110, 10110, 10111, 10101, 01111, 00111, 11100, 10000, 11001, 00010, 01010 -> 9" $ do
        let bitNumStrings = ["00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010"]
        let bitNums = parseBinNum <$> bitNumStrings
        (binNumToInt . getEpsilonRate) bitNums `shouldBe` 9
  describe "exercise 3.2" $ do
    describe "filterByMostCommon" $ do
      it "00100, 11110, 10110, 10111, 10101, 01111, 00111, 11100, 10000, 11001, 00010, 01010 -> 23" $ do
        let bitNumStrings =
              [ "00100",
                "11110",
                "10110",
                "10111",
                "10101",
                "01111",
                "00111",
                "11100",
                "10000",
                "11001",
                "00010",
                "01010"
              ]
        let bitNums = parseBinNum <$> bitNumStrings
        binNumToInt <$> filterByMostCommon 0 bitNums `shouldBe` [23]
    describe "filterByLeastCommon" $ do
      it "00100, 11110, 10110, 10111, 10101, 01111, 00111, 11100, 10000, 11001, 00010, 01010 -> 10" $ do
        let bitNumStrings =
              [ "00100",
                "11110",
                "10110",
                "10111",
                "10101",
                "01111",
                "00111",
                "11100",
                "10000",
                "11001",
                "00010",
                "01010"
              ]
        let bitNums = parseBinNum <$> bitNumStrings
        binNumToInt <$> filterByLeastCommon 0 bitNums `shouldBe` [10]