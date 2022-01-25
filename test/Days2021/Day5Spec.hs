{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Days2021.Day5Spec (spec) where

import Days2021.Day5
  ( calculateFirstResult,
    countAllOverlappingPoints,
    countOrthogonalOverlappingPoints,
    expandLinePoints,
    getLinePointsCount,
    getTotalPointsCount,
    isDiagonal,
    isOrthogonal,
    pair2Line,
    parsePair,
    renderLines,
  )
import Import
import qualified RIO.HashMap as HM
import Test.Hspec
import Util (getFilePath)

spec :: Spec
spec = do
  describe "parsePair" $ do
    it "'1,1 -> 1,3' -> ((1,1), (1,3))" $ do
      parsePair "1,1 -> 1,3" `shouldBe` ((1, 1), (1, 3))
  describe "expandLinePoints" $ do
    it "((1,1), (1,3)) -> [(1,1), (1,2), (1,3)]" $ do
      let l = (pair2Line . parsePair) "1,1 -> 1,3"
      expandLinePoints l
        `shouldBe` [ (1, 1),
                     (1, 2),
                     (1, 3)
                   ]
    it "((9,7), (7,7)) -> [(9,7), (8,7), (7,7)]" $ do
      let l = (pair2Line . parsePair) "9,7 -> 7,7"
      expandLinePoints l
        `shouldBe` [ (9, 7),
                     (8, 7),
                     (7, 7)
                   ]
    it "((0,0), (8,8)) -> [(0,0), ... (8,8)]" $ do
      let l = (pair2Line . parsePair) "0,0 -> 8,8"
      expandLinePoints l
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
      let l = (pair2Line . parsePair) "8,0 -> 0,8"
      expandLinePoints l
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
      let l = (pair2Line . parsePair) "6,4 -> 2,0"
      expandLinePoints l
        `shouldBe` [ (6, 4),
                     (5, 3),
                     (4, 2),
                     (3, 1),
                     (2, 0)
                   ]
  describe "isOrthogonal" $ do
    it "((1,1), (1,3)) -> True" $ do
      isOrthogonal ((1, 1), (1, 3)) `shouldBe` True
    it "((9,7), (7,7)) -> False" $ do
      isOrthogonal ((9, 6), (7, 7)) `shouldBe` False
  describe "getLinePointsCount" $ do
    it "counts point instances for one line" $ do
      let l = (pair2Line . parsePair) "10,7 -> 7,7"
      getLinePointsCount l
        `shouldBe` HM.fromList
          [ ((10, 7), 1),
            ((9, 7), 1),
            ((8, 7), 1),
            ((7, 7), 1)
          ]
  describe "getTotalPointsCount" $ do
    it "counts point instances for 2 lines" $ do
      let l0 = (pair2Line . parsePair) "10,7 -> 8,7"
      let l1 = (pair2Line . parsePair) "9,10 -> 9,6"
      getTotalPointsCount [l0, l1]
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
            parsePair
              <$> [ "0,1 -> 2,1",
                    "1,0 -> 1,2"
                  ]
      countOrthogonalOverlappingPoints parsedlines `shouldBe` 1
    it "should be 3 for 3 overlapping points" $ do
      let parsedlines =
            parsePair
              <$> [ "0,1 -> 2,1",
                    "1,0 -> 1,2",
                    "1,0 -> 1,6"
                  ]
      countOrthogonalOverlappingPoints parsedlines `shouldBe` 3
    it "counts the orthogonal overlapping points correctly for first example" $ do
      let parsedlines =
            parsePair
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
      countOrthogonalOverlappingPoints parsedlines `shouldBe` 5
    it "counts the all overlapping points correctly for second example" $ do
      let parsedlines =
            parsePair
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
      countAllOverlappingPoints parsedlines
        `shouldBe` 12
  describe "for actual input" $ do
    it "should not be 4531" $ do
      let filePath = getFilePath "21-5-1"
      firstResult <- calculateFirstResult filePath
      firstResult `shouldNotBe` "4531"
    it "should be 4655?" $ do
      let filePath = getFilePath "21-5-1"
      firstResult <- calculateFirstResult filePath
      firstResult `shouldBe` "4655"
  describe "isDiagonal" $ do
    it "0,0 -> 0,5 => False" $ do
      let l = parsePair "0,0 -> 0,5"
      isDiagonal l `shouldBe` False
    it "7,6 -> 0,6 => False" $ do
      let l = parsePair "7,6 -> 0,6"
      isDiagonal l `shouldBe` False
    it "7,0 -> 0,5 => False" $ do
      let l = parsePair "7,0 -> 0,5"
      isDiagonal l `shouldBe` False
    it "0,0 -> 5,5 => True" $ do
      let l = parsePair "0,0 -> 5,5"
      isDiagonal l `shouldBe` True
    it "7,7 -> 5,5 => True" $ do
      let l = parsePair "7,7 -> 5,5"
      isDiagonal l `shouldBe` True
    it "-7,7 -> -5,5 => True" $ do
      let l = parsePair "-7,7 -> -5,5"
      isDiagonal l
        `shouldBe` True
  describe "renderLines" $ do
    it "renders the 2nd example" $ do
      let parsedLines =
            pair2Line . parsePair
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
      renderLines parsedLines `shouldBe` expectedRender