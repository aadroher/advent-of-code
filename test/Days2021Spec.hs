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