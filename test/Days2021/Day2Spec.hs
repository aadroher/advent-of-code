{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Days2021.Day2Spec (spec) where

import Days2021.Day2 (Command (D, F, U), bearingMove, stepMove)
import Import
import Test.Hspec

spec :: Spec
spec = do
  describe "exercise 1" $ do
    describe "move" $ do
      it "(0, 0) -> [] -> (0, 0)" $ do
        stepMove (0, 0) [] `shouldBe` (0, 0)
      it "(0, 0) -> [F5, D5, F8, U3, D8, F2] -> (15, 10)" $ do
        stepMove
          (0, 0)
          [ F 5,
            D 5,
            F 8,
            U 3,
            D 8,
            F 2
          ]
          `shouldBe` (15, 10)
  describe "exercise 2" $ do
    describe "bearingMove" $ do
      it "(0, (0, 0)) -> [] -> (0, (0, 0))" $ do
        bearingMove (0, (0, 0)) [] `shouldBe` (0, (0, 0))
      it "(0, (0, 0)) -> [F5, D5, F8, U3, D8, F2] -> (10, (15, 60))" $ do
        bearingMove
          (0, (0, 0))
          [ F 5,
            D 5,
            F 8,
            U 3,
            D 8,
            F 2
          ]
          `shouldBe` (10, (15, 60))
