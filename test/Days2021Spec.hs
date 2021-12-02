{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Days2021Spec (spec) where

import qualified Days2021.Day1 as D1
import Import
import Test.Hspec

spec :: Spec
spec = do
  describe "AoC 2021" $ do
    describe "exercise 1.1" $ do
      it "[199, 200, 208, 210, 200, 207, 240, 269, 260, 263] -> 7" $
        D1.countIncreases [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]
          `shouldBe` 7