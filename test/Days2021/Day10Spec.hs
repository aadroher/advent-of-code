{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Days2021.Day10Spec (spec) where

import Days2021.Day10
  ( BrackeType (..),
    Chunk (..),
    ParseResult (..),
    Position (..),
    completeSequence,
    invert,
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
      it "[] -> Right ()" $ do
        (parseLine . lexLine) "[]" []
          `shouldBe` Ok
      it "{(([<>]))} -> Right ()" $ do
        (parseLine . lexLine) "{(([<>]))}" []
          `shouldBe` Ok
      it "{(([<>]))>([]) -> Left ('>', '}')" $ do
        (parseLine . lexLine) "{(([<>]))>([])" []
          `shouldBe` Corrupt (lexChunk '>', Just $ lexChunk '}')
      it "{([(<{}[<>[]}>{[]{[(<()> -> Left (']', '}')" $ do
        (parseLine . lexLine) "{([(<{}[<>[]}>{[]{[(<()>" []
          `shouldBe` Corrupt (Chunk Closing Curly, Just (Chunk Closing Square))
      it "<{([([[(<>()){}]>(<<{{ -> Left (']', '>')" $ do
        (parseLine . lexLine) "<{([([[(<>()){}]>(<<{{" []
          `shouldBe` Corrupt (lexChunk '>', Just (lexChunk ']'))
      it "[({(<(())[]>[[{[]{<()<>> -> Incomplete }}]])})]" $ do
        (parseLine . lexLine) "[({(<(())[]>[[{[]{<()<>>" []
          `shouldBe` Incomplete (invert . lexChunk <$> "}}]])})]")
    describe "completeSequence" $ do
      it "'[({(<(())[]>[[{[]{<()<>>' -> '}}]])})]'" $ do
        (completeSequence . lexLine) "[({(<(())[]>[[{[]{<()<>>"
          `shouldBe` lexLine "}}]])})]"
