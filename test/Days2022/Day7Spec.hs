{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Days2022.Day7Spec (spec) where

import Days2022.Day7
  ( Command (..),
    DirReference (..),
    ParsedLine (..),
    parseLine,
  )
import Import
import Test.Hspec

spec :: Spec
spec = do
  describe "exercise 1" $ do
    describe "parseLine" $ do
      it "$ cd / -> Cd Root" $ do
        parseLine "$ cd /" `shouldBe` ParsedCommand (Cd Root)
      it "$ cd .. -> Cd Parent" $ do
        parseLine "$ cd .." `shouldBe` ParsedCommand (Cd Parent)
      it "$ cd somedir -> Cd (Child 'somedir')" $ do
        parseLine "$ cd somedir" `shouldBe` ParsedCommand (Cd (Child "somedir"))