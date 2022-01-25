{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Days2021.Day6Spec (spec) where

import Days2021.Day6 (parseSchool, populationOnDayN)
import Import
import qualified RIO.HashMap as HM
import Test.Hspec

spec :: Spec
spec = do
  describe "parseSchool" $ do
    it "parses [3,4,3,1,2]" $ do
      parseSchool "3,4,3,1,2"
        `shouldBe` HM.fromList
          [ (0, 0),
            (1, 1),
            (2, 1),
            (3, 2),
            (4, 1),
            (5, 0),
            (6, 0),
            (7, 0),
            (8, 0)
          ]
  describe "populationOnDayN" $ do
    it "18 -> 26" $ do
      let initialSchool = parseSchool "3,4,3,1,2"
      populationOnDayN initialSchool 18 `shouldBe` 26
    it "80 -> 5934" $ do
      let initialSchool = parseSchool "3,4,3,1,2"
      populationOnDayN initialSchool 80 `shouldBe` 5934
    it "256 -> 26984457539" $ do
      let initialSchool = parseSchool "3,4,3,1,2"
      populationOnDayN initialSchool 256 `shouldBe` 26984457539
