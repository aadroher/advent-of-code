{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Days2022.Day3Spec (spec) where

import Days2022.Day3
  ( getRepeatedItemsPrioritySum,
    parseContentList,
  )
import Import
import Test.Hspec

itemLists =
  [ "vJrwpWtwJgWrhcsFMMfFFhFp",
    "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
    "PmmdzqPrVvPwwTWBwg",
    "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
    "ttgJtRGJQctTZtZT",
    "CrZsJsPPZsGzwwsLwLmpwMDw"
  ]

spec :: Spec
spec = do
  describe "exercise 1" $ do
    it "calculates the result for the example" $ do
      let parsedLists = parseContentList <$> itemLists
      getRepeatedItemsPrioritySum parsedLists `shouldBe` 157
