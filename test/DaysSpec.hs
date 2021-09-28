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
      it "1,0,0,0,99 -> 2,0,0,0,99" $ D2.executeIntCode [1, 0, 0, 0, 99] `shouldBe` [2, 0, 0, 0, 99]