{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Days2022.Day2Spec (spec) where

import Days2022.Day2
  ( Hand (..),
    MatchResult (..),
    MyHand (..),
    OponentHand (..),
    getMatchResult,
    parseMatch,
  )
import Import
import Test.Hspec

spec :: Spec
spec = do
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
