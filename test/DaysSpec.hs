{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module DaysSpec (spec) where

import Import
import qualified Import as D1
import Test.Hspec

spec :: Spec
spec = do
  describe "day 1" $ do
    it "should return 2 for 12" $ D1.getFuelRequirements 12 `shouldBe` 2
    it "should return 2 for 14" $ getFuelRequirements 14 `shouldBe` 2
    it "should return 654 for 1969" $ getFuelRequirements 1969 `shouldBe` 654
