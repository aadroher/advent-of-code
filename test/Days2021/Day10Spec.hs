{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Days2021.Day10Spec (spec) where

import Days2021.Day10
  ( BrackeType (..),
    Chunk (..),
    Position (..),
    lexChunk,
    lexLine,
    parseLine,
  )
import Import
import Test.Hspec

spec :: Spec
spec = do
  describe "exercise 1" $ do
    describe "lexChunk" $ do
      it "[ -> Opening Square" $ do
        lexChunk '[' `shouldBe` Chunk Opening Square
      it "} -> Closing Curly" $ do
        lexChunk '}' `shouldBe` Chunk Closing Curly
    describe "parseLine" $ do
      it "{([(<{}[<>[]}>{[]{[(<()> -> Left (']', '}')" $ do
        (parseLine . lexLine) "{([(<{}[<>[]}>{[]{[(<()>" []
          `shouldBe` (Left (Chunk Closing Square, Chunk Closing Curly))
