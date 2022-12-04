{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Days2022.Day2Spec (spec) where

import Days2022.Day2
  ( Hand (..),
    MatchResult (..),
    MyHand (..),
    OponentHand (..),
    getMatchResult,
    getMatchesScore,
    getPrescriptionsScore,
    parseMatch,
    parseMatchPrescription,
  )
import Import
import Test.Hspec

spec :: Spec
spec = do
  let textEntries =
        [ "A Y",
          "B X",
          "C Z"
        ]
  describe "exercise 1" $ do
    describe "getMatchResult" $ do
      it "Rock, Scissors -> First" $ do
        getMatchResult Rock Scissors `shouldBe` First
      it "Rock, Rock -> Draw" $ do
        getMatchResult Rock Rock `shouldBe` Draw
      it "Rock, Paper -> Second" $ do
        getMatchResult Rock Paper `shouldBe` Second
    describe "parseMatch" $ do
      it "A X -> (Rock, Rock)" $ do
        parseMatch "A X" `shouldBe` (OponentHand Rock, MyHand Rock)
      it "B Z -> (Paper, Scissors)" $ do
        parseMatch "B Z" `shouldBe` (OponentHand Paper, MyHand Scissors)
    describe "getMatchesScore" $ do
      it "solves the example" $ do
        let matches = parseMatch <$> textEntries
        getMatchesScore matches `shouldBe` 15
  describe "exercise 2" $ do
    describe "getPrescriptionsScore" $ do
      it "solves the example" $ do
        let matchPrescriptions = parseMatchPrescription <$> textEntries
        getPrescriptionsScore matchPrescriptions `shouldBe` 12
