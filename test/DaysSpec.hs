{-# LANGUAGE NoImplicitPrelude #-}

module DaysSpec (spec) where

import Import
import Test.Hspec
import Test.Hspec.QuickCheck
import Util

spec :: Spec
spec = do
  describe "day 1" $ do
    it "should return 2 for 12" $ getFuelRequirements 12 `shouldBe` 2
    it "should return 2 for 14" $ getFuelRequirements 14 `shouldBe` 2
    it "should return 654 for 1969" $ getFuelRequirements 1969 `shouldBe` 654
