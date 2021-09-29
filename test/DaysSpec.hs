{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module DaysSpec (spec) where

import qualified Days.Day1 as D1
import qualified Days.Day2 as D2
import Import
import Test.Hspec

spec :: Spec
spec = do
  describe "day 1" $ do
    describe "exercise 1.1" $ do
      it "should return 2 for 12" $ D1.getFuelRequirements 12 `shouldBe` 2
      it "should return 2 for 14" $ D1.getFuelRequirements 14 `shouldBe` 2
      it "should return 654 for 1969" $ D1.getFuelRequirements 1969 `shouldBe` 654
      it "should return 33583 for 100756" $ D1.getFuelRequirements 100756 `shouldBe` 33583
    describe "exercise 1.2" $ do
      it "should return 2 for 14" $ D1.getTotalFuelRequirements 14 `shouldBe` 2
      it "should return 966 for 1969" $ D1.getTotalFuelRequirements 1969 `shouldBe` 966
      it "should return 50346 for 100756" $ D1.getTotalFuelRequirements 100756 `shouldBe` 50346
  describe "day 2" $ do
    describe "exercise 2.1" $ do
      it "1,0,0,0,99 -> 2,0,0,0,99" $
        D2.executeIntCode [1, 0, 0, 0, 99] `shouldBe` [2, 0, 0, 0, 99]
      it "2,3,0,3,99 -> 2,3,0,6,99" $
        D2.executeIntCode [2, 3, 0, 3, 99] `shouldBe` [2, 3, 0, 6, 99]
      it "2,4,4,5,99,0 -> 2,3,0,6,99" $
        D2.executeIntCode [2, 4, 4, 5, 99, 0] `shouldBe` [2, 4, 4, 5, 99, 9801]
      it "1,1,1,4,99,5,6,0,99 -> 30,1,1,4,2,5,6,0,99" $
        D2.executeIntCode [1, 1, 1, 4, 99, 5, 6, 0, 99] `shouldBe` [30, 1, 1, 4, 2, 5, 6, 0, 99]
    describe "exercise 2.2" $ do
      it "(12, 2) -> 3058646" $ do
        initialMemory <- D2.loadData "./data/day2.txt"
        D2.getResult initialMemory 12 2 `shouldBe` 3058646
      it "(12, 2) -> 3058646 is True" $ do
        initialMemory <- D2.loadData "./data/day2.txt"
        D2.resultIn initialMemory 12 2 3058646 `shouldBe` True
