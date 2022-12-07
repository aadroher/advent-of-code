{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Days2022.Day3Spec (spec) where

import Days2022.Day3
  ( Item (..),
    getBadge,
    getDuplicatedItem,
    getItemPriority,
    getRepeatedItemsPrioritySum,
    getRuckSackGroups,
    parseContentList,
  )
import Import
import Test.Hspec

itemLists :: [Text]
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
    describe "parseContentList" $ do
      it "'ab' -> [Item 'a', Item 'b']" $ do
        parseContentList "ab" `shouldBe` [Item 'a', Item 'b']
    describe "getDuplicatedItem" $ do
      it "'abcb' -> Item 'b'" $ do
        let parsedList = parseContentList "abcb"
        getDuplicatedItem parsedList `shouldBe` Item 'b'
    describe "getItemPriority" $ do
      it "Item 'C' -> 29" $ do
        getItemPriority (Item 'C') `shouldBe` 29
    describe "getRuckSackGroups" $ do
      it "works with the example" $ do
        let parsedLists = parseContentList <$> itemLists
        let expectedGroups =
              [ ( Item <$> "vJrwpWtwJgWrhcsFMMfFFhFp",
                  Item <$> "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
                  Item <$> "PmmdzqPrVvPwwTWBwg"
                ),
                ( Item <$> "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
                  Item <$> "ttgJtRGJQctTZtZT",
                  Item <$> "CrZsJsPPZsGzwwsLwLmpwMDw"
                )
              ]
        getRuckSackGroups parsedLists `shouldBe` expectedGroups
    describe "getBadge" $ do
      it "calculates the badge for the first group" $ do
        let rucksackGroup =
              ( Item <$> "vJrwpWtwJgWrhcsFMMfFFhFp",
                Item <$> "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
                Item <$> "PmmdzqPrVvPwwTWBwg"
              )
        getBadge rucksackGroup `shouldBe` Item 'r'
      it "calculates the badge for the second group" $ do
        let rucksackGroup =
              ( Item <$> "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
                Item <$> "ttgJtRGJQctTZtZT",
                Item <$> "CrZsJsPPZsGzwwsLwLmpwMDw"
              )
        getBadge rucksackGroup `shouldBe` Item 'Z'

    it "calculates the result for the example" $ do
      let parsedLists = parseContentList <$> itemLists
      getRepeatedItemsPrioritySum parsedLists `shouldBe` 157
