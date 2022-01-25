{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Days2021.Day7Spec (spec) where

import Days2021.Day7
  ( getNaiveOptimalAlignment,
    getWeightedOptimalAlignment,
  )
import Import
import Test.Hspec

spec :: Spec
spec = do
  describe "optimalAlignment" $ do
    it "16,1,2,0,4,2,7,1,2,14 -> (2, 37)" $ do
      let initialPositions =
            [ 16,
              1,
              2,
              0,
              4,
              2,
              7,
              1,
              2,
              14
            ]
      getNaiveOptimalAlignment initialPositions `shouldBe` (2, 37)
  describe "optimalAlignment" $ do
    it "16,1,2,0,4,2,7,1,2,14 -> (5, 168)" $ do
      let initialPositions =
            [ 16,
              1,
              2,
              0,
              4,
              2,
              7,
              1,
              2,
              14
            ]
      getWeightedOptimalAlignment initialPositions `shouldBe` (5, 168)
