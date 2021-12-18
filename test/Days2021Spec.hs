{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Days2021Spec (spec) where

import qualified Data.List as L
import qualified Days2021.Day1 as D1
import Days2021.Day2 (Command (..))
import qualified Days2021.Day2 as D2
import Days2021.Day3 (Bit (..))
import qualified Days2021.Day3 as D3
import Days2021.Day4 (Game (..))
import qualified Days2021.Day4 as D4
import qualified Days2021.Day5 as D5
import qualified Days2021.Day6 as D6
import Import
import qualified RIO.HashMap as HM
import qualified RIO.Set as S
import Test.Hspec
import Text.Pretty.Simple (pPrint)
import Util (getFilePath)

spec :: Spec
spec = do
  describe "AoC 2021" $ do
    describe "exercise 1.1" $ do
      describe "countIncreases" $ do
        it "[199, 200, 208, 210, 200, 207, 240, 269, 260, 263] -> 7" $
          D1.countIncreases [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]
            `shouldBe` 7
    describe "exercise 1.2" $ do
      describe "generateWindows" $ do
        it "[199, 200, 208, 210, 200, 207, 240, 269] -> [(199, 200, 208) ... (269, 0, 0)]" $
          D1.generateWindows [199, 200, 208, 210, 200, 207, 240, 269]
            `shouldBe` [ (199, 200, 208),
                         (200, 208, 210),
                         (208, 210, 200),
                         (210, 200, 207),
                         (200, 207, 240),
                         (207, 240, 269),
                         (240, 269, 0),
                         (269, 0, 0)
                       ]
      describe "countWindowIncreases" $ do
        it "[199, 200, 208, 210, 200, 207, 240, 269, 260, 263] -> 5" $ do
          D1.countWindowIncreases [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]
            `shouldBe` 5
    describe "exercise 2.1" $ do
      describe "move" $ do
        it "(0, 0) -> [] -> (0, 0)" $ do
          D2.stepMove (0, 0) [] `shouldBe` (0, 0)
        it "(0, 0) -> [F5, D5, F8, U3, D8, F2] -> (15, 10)" $ do
          D2.stepMove (0, 0) [F 5, D 5, F 8, U 3, D 8, F 2] `shouldBe` (15, 10)
    describe "exercise 2.2" $ do
      describe "bearingMove" $ do
        it "(0, (0, 0)) -> [] -> (0, (0, 0))" $ do
          D2.bearingMove (0, (0, 0)) [] `shouldBe` (0, (0, 0))
        it "(0, (0, 0)) -> [F5, D5, F8, U3, D8, F2] -> (10, (15, 60))" $ do
          D2.bearingMove (0, (0, 0)) [F 5, D 5, F 8, U 3, D 8, F 2] `shouldBe` (10, (15, 60))
    describe "exercise 3.1" $ do
      describe "getMostCommon" $ do
        it "[0,0,1] -> 0" $ do
          D3.getMostCommon [Zero, Zero, One] `shouldBe` Zero
        it "[0,1,1] -> 0" $ do
          D3.getMostCommon [Zero, One, One] `shouldBe` One
        it "[0,0,1,1] -> 0" $ do
          D3.getMostCommon [Zero, Zero, One, One] `shouldBe` One
      describe "getLeastCommon" $ do
        it "[0,0,1] -> 0" $ do
          D3.getLeastCommon [Zero, Zero, One] `shouldBe` One
        it "[0,1,1] -> 0" $ do
          D3.getLeastCommon [Zero, One, One] `shouldBe` Zero
        it "[0,0,1,1] -> 0" $ do
          D3.getLeastCommon [Zero, Zero, One, One] `shouldBe` Zero
      describe "binNumToInt" $ do
        it "10110 -> 22" $ do
          let binNum = D3.parseBinNum "10110"
          D3.binNumToInt binNum `shouldBe` 22
      describe "getGammaRate" $ do
        it "00100, 11110, 10110, 10111, 10101, 01111, 00111, 11100, 10000, 11001, 00010, 01010 -> 22" $ do
          let bitNumStrings = ["00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010"]
          let bitNums = D3.parseBinNum <$> bitNumStrings
          (D3.binNumToInt . D3.getGammaRate) bitNums `shouldBe` 22
      describe "getEpsilonRate" $ do
        it "00100, 11110, 10110, 10111, 10101, 01111, 00111, 11100, 10000, 11001, 00010, 01010 -> 9" $ do
          let bitNumStrings = ["00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010"]
          let bitNums = D3.parseBinNum <$> bitNumStrings
          (D3.binNumToInt . D3.getEpsilonRate) bitNums `shouldBe` 9
    describe "exercise 3.2" $ do
      describe "filterByMostCommon" $ do
        it "00100, 11110, 10110, 10111, 10101, 01111, 00111, 11100, 10000, 11001, 00010, 01010 -> 23" $ do
          let bitNumStrings =
                [ "00100",
                  "11110",
                  "10110",
                  "10111",
                  "10101",
                  "01111",
                  "00111",
                  "11100",
                  "10000",
                  "11001",
                  "00010",
                  "01010"
                ]
          let bitNums = D3.parseBinNum <$> bitNumStrings
          D3.binNumToInt <$> D3.filterByMostCommon 0 bitNums `shouldBe` [23]
      describe "filterByLeastCommon" $ do
        it "00100, 11110, 10110, 10111, 10101, 01111, 00111, 11100, 10000, 11001, 00010, 01010 -> 10" $ do
          let bitNumStrings =
                [ "00100",
                  "11110",
                  "10110",
                  "10111",
                  "10101",
                  "01111",
                  "00111",
                  "11100",
                  "10000",
                  "11001",
                  "00010",
                  "01010"
                ]
          let bitNums = D3.parseBinNum <$> bitNumStrings
          D3.binNumToInt <$> D3.filterByLeastCommon 0 bitNums `shouldBe` [10]
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
          D4.getColumn board 2 `shouldBe` [17, 23, 14, 3, 20]
      describe "isWinningLine" $ do
        let line = [17, 23, 14, 3, 20]
        it "returns false for no called numbers" $ do
          let calledNumbers = []
          D4.isWinningLine calledNumbers line `shouldBe` False
      describe "isWinningBoard" $ do
        it "returns false for no called numbers" $ do
          let calledNumbers = []
          D4.isWinningBoard calledNumbers board `shouldBe` False
        it "returns false for a partial match" $ do
          let calledNumbers = [17, 14, 3, 5, 19]
          D4.isWinningBoard calledNumbers board `shouldBe` False
        it "returns true for a matched row" $ do
          let calledNumbers = [8, 2, 23, 4, 24]
          D4.isWinningBoard calledNumbers board `shouldBe` True
        it "returns true for a matched column" $ do
          let calledNumbers = [17, 23, 14, 3, 20]
          D4.isWinningBoard calledNumbers board `shouldBe` True
        it "returns true for a matched row with additional numbers" $ do
          let calledNumbers = [8, 17, 2, 33, 21, 23, 4, 19, 24]
          D4.isWinningBoard calledNumbers board `shouldBe` True
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
          let winningSequence = D4.getWinningSequence [b0, b1, b2] numbersToCall ([], [])
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
          D4.getScore sequenceToWinningMove b `shouldBe` 4512
      describe "parseCalledNumbers" $ do
        it "parses 7,4,9,5,11,17,23,2,0,14,21,24" $ do
          D4.parseCalledNumbers "7,4,9,5,11,17,23,2,0,14,21,24" `shouldBe` [7, 4, 9, 5, 11, 17, 23, 2, 0, 14, 21, 24]
      describe "parseFigure" $ do
        it "parses 22" $ do
          D4.parseFigure "22" `shouldBe` 22
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
          D4.parseBoard t `shouldBe` expectedBoard
    describe "exercise 5" $ do
      describe "parsePair" $ do
        it "'1,1 -> 1,3' -> ((1,1), (1,3))" $ do
          D5.parsePair "1,1 -> 1,3" `shouldBe` ((1, 1), (1, 3))
      describe "expandLinePoints" $ do
        it "((1,1), (1,3)) -> [(1,1), (1,2), (1,3)]" $ do
          let l = (D5.pair2Line . D5.parsePair) "1,1 -> 1,3"
          D5.expandLinePoints l
            `shouldBe` [ (1, 1),
                         (1, 2),
                         (1, 3)
                       ]
        it "((9,7), (7,7)) -> [(9,7), (8,7), (7,7)]" $ do
          let l = (D5.pair2Line . D5.parsePair) "9,7 -> 7,7"
          D5.expandLinePoints l
            `shouldBe` [ (9, 7),
                         (8, 7),
                         (7, 7)
                       ]
        it "((0,0), (8,8)) -> [(0,0), ... (8,8)]" $ do
          let l = (D5.pair2Line . D5.parsePair) "0,0 -> 8,8"
          D5.expandLinePoints l
            `shouldBe` [ (0, 0),
                         (1, 1),
                         (2, 2),
                         (3, 3),
                         (4, 4),
                         (5, 5),
                         (6, 6),
                         (7, 7),
                         (8, 8)
                       ]
        it "((8,0), (0,8)) -> [(8,0), ... (0,8)]" $ do
          let l = (D5.pair2Line . D5.parsePair) "8,0 -> 0,8"
          D5.expandLinePoints l
            `shouldBe` [ (8, 0),
                         (7, 1),
                         (6, 2),
                         (5, 3),
                         (4, 4),
                         (3, 5),
                         (2, 6),
                         (1, 7),
                         (0, 8)
                       ]
        it "((6,4), (2,0)) -> [(6,4), (5, 3) ... (2,0)]" $ do
          let l = (D5.pair2Line . D5.parsePair) "6,4 -> 2,0"
          D5.expandLinePoints l
            `shouldBe` [ (6, 4),
                         (5, 3),
                         (4, 2),
                         (3, 1),
                         (2, 0)
                       ]
      describe "isOrthogonal" $ do
        it "((1,1), (1,3)) -> True" $ do
          D5.isOrthogonal ((1, 1), (1, 3)) `shouldBe` True
        it "((9,7), (7,7)) -> False" $ do
          D5.isOrthogonal ((9, 6), (7, 7)) `shouldBe` False
      describe "getLinePointsCount$" $ do
        it "counts point instances for one line" $ do
          let l = (D5.pair2Line . D5.parsePair) "10,7 -> 7,7"
          D5.getLinePointsCount l
            `shouldBe` HM.fromList
              [ ((10, 7), 1),
                ((9, 7), 1),
                ((8, 7), 1),
                ((7, 7), 1)
              ]
      describe "getTotalPointsCount" $ do
        it "counts point instances for 2 lines" $ do
          let l0 = (D5.pair2Line . D5.parsePair) "10,7 -> 8,7"
          let l1 = (D5.pair2Line . D5.parsePair) "9,10 -> 9,6"
          D5.getTotalPointsCount [l0, l1]
            `shouldBe` HM.fromList
              [ ((9, 10), 1),
                ((9, 9), 1),
                ((9, 8), 1),
                ((10, 7), 1),
                ((9, 7), 2),
                ((9, 6), 1),
                ((8, 7), 1)
              ]
      describe "countOrthogonalOverlappingPoints" $ do
        it "should work for 1 overlapping point" $ do
          let parsedlines =
                D5.parsePair
                  <$> [ "0,1 -> 2,1",
                        "1,0 -> 1,2"
                      ]
          D5.countOrthogonalOverlappingPoints parsedlines `shouldBe` 1
        it "should be 3 for 3 overlapping points" $ do
          let parsedlines =
                D5.parsePair
                  <$> [ "0,1 -> 2,1",
                        "1,0 -> 1,2",
                        "1,0 -> 1,6"
                      ]
          D5.countOrthogonalOverlappingPoints parsedlines `shouldBe` 3
        it "counts the orthogonal overlapping points correctly for first example" $ do
          let parsedlines =
                D5.parsePair
                  <$> [ "0,9 -> 5,9",
                        "8,0 -> 0,8",
                        "9,4 -> 3,4",
                        "2,2 -> 2,1",
                        "7,0 -> 7,4",
                        "6,4 -> 2,0",
                        "0,9 -> 2,9",
                        "3,4 -> 1,4",
                        "0,0 -> 8,8",
                        "5,5 -> 8,2"
                      ]
          D5.countOrthogonalOverlappingPoints parsedlines `shouldBe` 5
        it "counts the all overlapping points correctly for second example" $ do
          let parsedlines =
                D5.parsePair
                  <$> [ "0,9 -> 5,9",
                        "8,0 -> 0,8",
                        "9,4 -> 3,4",
                        "2,2 -> 2,1",
                        "7,0 -> 7,4",
                        "6,4 -> 2,0",
                        "0,9 -> 2,9",
                        "3,4 -> 1,4",
                        "0,0 -> 8,8",
                        "5,5 -> 8,2"
                      ]
          D5.countAllOverlappingPoints parsedlines `shouldBe` 12
        describe "for actual input" $ do
          it "should not be 4531" $ do
            let filePath = getFilePath "21-5-1"
            firstResult <- D5.calculateFirstResult filePath
            firstResult `shouldNotBe` "4531"
          it "should be 4655?" $ do
            let filePath = getFilePath "21-5-1"
            firstResult <- D5.calculateFirstResult filePath
            firstResult `shouldBe` "4655"
      describe "isDiagonal" $ do
        it "0,0 -> 0,5 => False" $ do
          let l = D5.parsePair "0,0 -> 0,5"
          D5.isDiagonal l `shouldBe` False
        it "7,6 -> 0,6 => False" $ do
          let l = D5.parsePair "7,6 -> 0,6"
          D5.isDiagonal l `shouldBe` False
        it "7,0 -> 0,5 => False" $ do
          let l = D5.parsePair "7,0 -> 0,5"
          D5.isDiagonal l `shouldBe` False
        it "0,0 -> 5,5 => True" $ do
          let l = D5.parsePair "0,0 -> 5,5"
          D5.isDiagonal l `shouldBe` True
        it "7,7 -> 5,5 => True" $ do
          let l = D5.parsePair "7,7 -> 5,5"
          D5.isDiagonal l `shouldBe` True
        it "-7,7 -> -5,5 => True" $ do
          let l = D5.parsePair "-7,7 -> -5,5"
          D5.isDiagonal l `shouldBe` True
      describe "renderLines" $ do
        it "renders the 2nd example" $ do
          let parsedLines =
                (D5.pair2Line . D5.parsePair)
                  <$> [ "0,9 -> 5,9",
                        "8,0 -> 0,8",
                        "9,4 -> 3,4",
                        "2,2 -> 2,1",
                        "7,0 -> 7,4",
                        "6,4 -> 2,0",
                        "0,9 -> 2,9",
                        "3,4 -> 1,4",
                        "0,0 -> 8,8",
                        "5,5 -> 8,2"
                      ]
          let expectedRender =
                "\n\
                \1.1....11.\n\
                \.111...2..\n\
                \..2.1.111.\n\
                \...1.2.2..\n\
                \.112313211\n\
                \...1.2....\n\
                \..1...1...\n\
                \.1.....1..\n\
                \1.......1.\n\
                \222111....\n"
          D5.renderLines parsedLines `shouldBe` expectedRender
    fdescribe "exercise 6" $ do
      describe "firstNDays" $ do
        it "(3,4,3,1,2),0 -> (3,4,3,1,2)" $ do
          let initialSchool = [3, 4, 3, 1, 2]
          let expectedFinalSchool = [[3, 4, 3, 1, 2]]
          D6.firstNDays 0 initialSchool `shouldBe` expectedFinalSchool
        it "(3,4,3,1,2),1 -> (2,3,2,0,1)" $ do
          let initialSchool = [3, 4, 3, 1, 2]
          let expectedFinalSchool = [[3, 4, 3, 1, 2], [2, 3, 2, 0, 1]]
          D6.firstNDays 1 initialSchool `shouldBe` expectedFinalSchool
      describe "dayNSchool" $ do
        it "(3,4,3,1,2),0 -> (3,4,3,1,2)" $ do
          let initialSchool = [3, 4, 3, 1, 2]
          let expectedFinalSchool = [3, 4, 3, 1, 2]
          D6.dayNSchool initialSchool 0 `shouldBe` expectedFinalSchool
        it "(3,4,3,1,2),1 -> (2,3,2,0,1)" $ do
          let initialSchool = [3, 4, 3, 1, 2]
          let expectedFinalSchool = [2, 3, 2, 0, 1]
          D6.dayNSchool initialSchool 1 `shouldBe` expectedFinalSchool
        it "(3,4,3,1,2),2 -> (1,2,1,6,0,8)" $ do
          let initialSchool = [3, 4, 3, 1, 2]
          let expectedFinalSchool = [1, 2, 1, 6, 0, 8]
          D6.dayNSchool initialSchool 2 `shouldBe` expectedFinalSchool
        it "(3,4,3,1,2),4 -> (0,1,0,5,6,0,1,2,2,3,7,8)" $ do
          let initialSchool = [3, 4, 3, 1, 2]
          let expectedFinalSchool = [0, 1, 0, 5, 6, 0, 1, 2, 2, 3, 7, 8]
          D6.dayNSchool initialSchool 10 `shouldBe` expectedFinalSchool
      fdescribe "populationOnDayN" $ do
        it "18 -> 26" $ do
          let initialSchool = [3, 4, 3, 1, 2]
          D6.populationOnDayN initialSchool 18 `shouldBe` 26
        it "80 -> 5934" $ do
          let initialSchool = [3, 4, 3, 1, 2]
          D6.populationOnDayN initialSchool 80 `shouldBe` 5934