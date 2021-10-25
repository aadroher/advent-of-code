{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module DaysSpec (spec) where

import qualified Days.Day1 as D1
import qualified Days.Day2 as D2
import Days.Day3 (Direction (..))
import qualified Days.Day3 as D3
import Import
import qualified RIO.Set as S
import Test.Hspec

spec :: Spec
spec = do
  describe "day 1" $ do
    describe "exercise 1.1" $ do
      it "should return 2 for 12" $ D1.getFuelRequirements 12 `shouldBe` 2
      it "should return 2 for 14" $ D1.getFuelRequirements 14 `shouldBe` 2
      it "should return 654 for 1969" $ D1.getFuelRequirements 1969 `shouldBe` 654
      it "should return 33583 for 100756" $ D1.getFuelRequirements 100756 `shouldBe` 33583
    describe "exercise 1.2" $ do
      it "should return 2 for 14" $ D1.getTotalFuelRequirements 14 `shouldBe` 2
      it "should return 966 for 1969" $ D1.getTotalFuelRequirements 1969 `shouldBe` 966
      it "should return 50346 for 100756" $ D1.getTotalFuelRequirements 100756 `shouldBe` 50346
  describe "day 2" $ do
    describe "exercise 2.1" $ do
      it "1,0,0,0,99 -> 2,0,0,0,99" $
        D2.executeIntCode [1, 0, 0, 0, 99] `shouldBe` [2, 0, 0, 0, 99]
      it "2,3,0,3,99 -> 2,3,0,6,99" $
        D2.executeIntCode [2, 3, 0, 3, 99] `shouldBe` [2, 3, 0, 6, 99]
      it "2,4,4,5,99,0 -> 2,3,0,6,99" $
        D2.executeIntCode [2, 4, 4, 5, 99, 0] `shouldBe` [2, 4, 4, 5, 99, 9801]
      it "1,1,1,4,99,5,6,0,99 -> 30,1,1,4,2,5,6,0,99" $
        D2.executeIntCode [1, 1, 1, 4, 99, 5, 6, 0, 99] `shouldBe` [30, 1, 1, 4, 2, 5, 6, 0, 99]
    describe "exercise 2.2" $ do
      it "(12, 2) -> 3058646" $ do
        initialMemory <- D2.loadData "./data/day2.txt"
        D2.getResult initialMemory 12 2 `shouldBe` 3058646
      it "(12, 2) -> 3058646 is True" $ do
        initialMemory <- D2.loadData "./data/day2.txt"
        D2.resultIn initialMemory 12 2 3058646 `shouldBe` True
  describe "day 3" $ do
    describe "exercise 3.1" $ do
      describe "getRoute" $ do
        it "[R2,U1] -> [(0,0), (1,0), (2,0), (2, 1)]" $ do
          D3.getRoute [(0, 0)] [(R, 2), (U, 1)] `shouldBe` [(0, 0), (1, 0), (2, 0), (2, 1)]
      describe "getPerimeter" $ do
        it "(0, 0) -> 1 -> [(1, 0), (0, -1), (-1, 0), (0, 1)]" $ do
          D3.getPerimeter (0, 0) 1 `shouldBe` S.fromList [(1, 0), (0, -1), (-1, 0), (0, 1)]
      describe "parseInstructions" $ do
        it "parses [R75,D30,R83,U83,L12,D49,R71,U7,L72]" $ do
          let expected = [(R, 75), (D, 30), (R, 83), (U, 83), (L, 12), (D, 49), (R, 71), (U, 7), (L, 72)]
          D3.parseInstructions "R75,D30,R83,U83,L12,D49,R71,U7,L72" `shouldBe` expected
        it "parses [U62,R66,U55,R34,D71,R55,D58,R83]" $ do
          let expected = [(U, 62), (R, 66), (U, 55), (R, 34), (D, 71), (R, 55), (D, 58), (R, 83)]
          D3.parseInstructions "U62,R66,U55,R34,D71,R55,D58,R83" `shouldBe` expected
      describe "getDistToClosestIntersection" $ do
        it "([R8,U5,L5,D3], [U7,R6,D4,L4]) -> 6" $ do
          let p = [(R, 8), (U, 5), (L, 5), (D, 3)]
          let q = [(U, 7), (R, 6), (D, 4), (L, 4)]
          D3.getDistToClosestIntersection p q `shouldBe` 6
        it "([R75,D30,R83,U83,L12,D49,R71,U7,L72], [U62,R66,U55,R34,D71,R55,D58,R83]) -> 159" $ do
          let p = D3.parseInstructions "R75,D30,R83,U83,L12,D49,R71,U7,L72"
          let q = D3.parseInstructions "U62,R66,U55,R34,D71,R55,D58,R83"
          D3.getDistToClosestIntersection p q `shouldBe` 159
        it "([R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51], [U98,R91,D20,R16,D67,R40,U7,R15,U6,R7]) -> 135" $ do
          let p = D3.parseInstructions "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
          let q = D3.parseInstructions "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
          D3.getDistToClosestIntersection p q `shouldBe` 135