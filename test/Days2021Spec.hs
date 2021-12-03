{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Days2021Spec (spec) where

-- import Days2021.Day1 (Window)
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
    describe "exercise 1.2" $ do
      it "[199, 200, 208, 210, 200, 207, 240, 269] -> [{0, [199, 200, 208]} ... {2, [208, 0, 0]}]" $
        D1.generateWindows [199, 200, 208, 210, 200, 207, 240, 269]
          `shouldBe` [ (0, (199, 200, 208)),
                       (1, (200, 208, 210)),
                       (2, (208, 210, 200)),
                       (3, (210, 200, 207)),
                       (4, (200, 207, 240)),
                       (5, (207, 240, 269)),
                       (6, (240, 269, 0)),
                       (7, (269, 0, 0))
                     ]