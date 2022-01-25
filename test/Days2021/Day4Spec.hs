{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Days2021.Day4Spec (spec) where

import Days2021.Day4
  ( getColumn,
    getScore,
    getWinningSequence,
    isWinningBoard,
    isWinningLine,
    parseBoard,
    parseCalledNumbers,
    parseFigure,
  )
import Import
import Test.Hspec

spec :: Spec
spec = do
  describe "AoC 2021 Day 4" $ do
    describe "exercise 4" $ do
      let board =
            [ [22, 13, 17, 11, 0],
              [8, 2, 23, 4, 24],
              [21, 9, 14, 16, 7],
              [6, 10, 3, 18, 5],
              [1, 12, 20, 15, 19]
            ]
      describe "getColumn" $ do
        it "retrieves the right column" $ do
          getColumn board 2 `shouldBe` [17, 23, 14, 3, 20]
      describe "isWinningLine" $ do
        let line = [17, 23, 14, 3, 20]
        it "returns false for no called numbers" $ do
          let calledNumbers = []
          isWinningLine calledNumbers line `shouldBe` False
      describe "isWinningBoard" $ do
        it "returns false for no called numbers" $ do
          let calledNumbers = []
          isWinningBoard calledNumbers board `shouldBe` False
        it "returns false for a partial match" $ do
          let calledNumbers = [17, 14, 3, 5, 19]
          isWinningBoard calledNumbers board `shouldBe` False
        it "returns true for a matched row" $ do
          let calledNumbers = [8, 2, 23, 4, 24]
          isWinningBoard calledNumbers board `shouldBe` True
        it "returns true for a matched column" $ do
          let calledNumbers = [17, 23, 14, 3, 20]
          isWinningBoard calledNumbers board `shouldBe` True
        it "returns true for a matched row with additional numbers" $ do
          let calledNumbers = [8, 17, 2, 33, 21, 23, 4, 19, 24]
          isWinningBoard calledNumbers board `shouldBe` True
      describe "getWinningSequence" $ do
        let numbersToCall = [7, 4, 9, 5, 11, 17, 23, 2, 0, 14, 21, 24, 10, 16, 13, 6, 15, 25, 12, 22, 18, 20, 8, 19, 3, 26, 1]
        let b0 =
              [ [22, 13, 17, 11, 0],
                [8, 2, 23, 4, 24],
                [21, 9, 14, 16, 7],
                [6, 10, 3, 18, 5],
                [1, 12, 20, 15, 19]
              ]
        let b1 =
              [ [3, 15, 0, 2, 22],
                [9, 18, 13, 17, 5],
                [19, 8, 7, 25, 23],
                [20, 11, 10, 24, 4],
                [14, 21, 16, 12, 6]
              ]
        let b2 =
              [ [14, 21, 17, 24, 4],
                [10, 16, 15, 9, 19],
                [18, 8, 23, 26, 20],
                [22, 11, 13, 6, 5],
                [2, 0, 12, 3, 7]
              ]
        it "returns the winning boards in the right order" $ do
          let winningSequence = getWinningSequence [b0, b1, b2] numbersToCall ([], [])
          winningSequence `shouldBe` [(b2, 4512), (b0, 2192), (b1, 1924)]
      describe "getScore" $ do
        let b =
              [ [14, 21, 17, 24, 4],
                [10, 16, 15, 9, 19],
                [18, 8, 23, 26, 20],
                [22, 11, 13, 6, 5],
                [2, 0, 12, 3, 7]
              ]
        let sequenceToWinningMove = reverse [7, 4, 9, 5, 11, 17, 23, 2, 0, 14, 21, 24]
        it "returns 4512 for the previous winning board" $ do
          getScore sequenceToWinningMove b `shouldBe` 4512
      describe "parseCalledNumbers" $ do
        it "parses 7,4,9,5,11,17,23,2,0,14,21,24" $ do
          parseCalledNumbers "7,4,9,5,11,17,23,2,0,14,21,24" `shouldBe` [7, 4, 9, 5, 11, 17, 23, 2, 0, 14, 21, 24]
      describe "parseFigure" $ do
        it "parses 22" $ do
          parseFigure "22" `shouldBe` 22
      describe "parseBoard" $ do
        it "parses board" $ do
          let t =
                "22 13 17 11  0\n\
                \ 8  2 23  4 24\n\
                \21  9 14 16  7\n\
                \ 6 10  3 18  5\n\
                \ 1 12 20 15 19"
          let expectedBoard =
                [ [22, 13, 17, 11, 0],
                  [8, 2, 23, 4, 24],
                  [21, 9, 14, 16, 7],
                  [6, 10, 3, 18, 5],
                  [1, 12, 20, 15, 19]
                ]
          parseBoard t `shouldBe` expectedBoard
