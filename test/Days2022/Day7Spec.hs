{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Days2022.Day7Spec (spec) where

import Days2022.Day7
  ( Command (..),
    DirReference (..),
    Node (..),
    ParsedLine (..),
    parseLine,
  )
import Import
import Test.Hspec

spec :: Spec
spec = do
  describe "exercise 1" $ do
    describe "parseLine" $ do
      it "$ cd / -> ParsedCommand (Cd Root)" $ do
        parseLine "$ cd /" `shouldBe` ParsedCommand (Cd Root)
      it "$ cd .. -> ParsedCommand (Cd Parent)" $ do
        parseLine "$ cd .." `shouldBe` ParsedCommand (Cd Parent)
      it "$ cd somedir -> ParsedCommand (Cd (Child 'somedir'))" $ do
        parseLine "$ cd somedir" `shouldBe` ParsedCommand (Cd (Child "somedir"))
      it "$ ls -> ParsedCommand Ls" $ do
        parseLine "$ ls" `shouldBe` ParsedCommand Ls
      it "dir somedir -> ParsedNode (Dir 'somedir')" $ do
        parseLine "dir somedir" `shouldBe` ParsedNode (Dir "somedir")
      it "62596 h.lst -> ParsedNode (File 62596 'h.lst')" $ do
        parseLine "62596 h.lst" `shouldBe` ParsedNode (File 62596 "h.lst")