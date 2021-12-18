{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Days2021.Day6 where

import Data.List (iterate')
import Import
import qualified RIO.List as L
import qualified RIO.List.Partial as L'
import RIO.Partial (read)
import qualified RIO.Text as T
import Util (calculateResult)

type Fish = Int

type FishSchool = [Fish]

reproductionPeriod :: Int
reproductionPeriod = 7

parseSchool :: Text -> FishSchool
parseSchool t = read . T.unpack <$> T.split (== ',') t

shouldSpawn :: Fish -> Bool
shouldSpawn 0 = True
shouldSpawn _ = False

nextDayFish :: Fish -> Fish
nextDayFish f = bool (f - 1) 6 (f == 0)

nextDaySchool :: FishSchool -> FishSchool
nextDaySchool fs =
  L.foldl'
    (\newFishes f -> bool newFishes (8 : newFishes) $ shouldSpawn f)
    []
    fs
    ++ existingFishes
  where
    existingFishes = nextDayFish <$> fs

firstNDays :: Int -> FishSchool -> [FishSchool]
firstNDays n fs = L.take (n + 1) $ iterate' nextDaySchool fs

dayNSchool :: FishSchool -> Int -> FishSchool
dayNSchool fs n = L.foldl' (\fs' _ -> nextDaySchool fs') fs [1 .. n]

numChildrenOnDayN :: Fish -> Int -> Int
-- numChildrenOnDayN s n = (n - s - 1 + reproductionPeriod) `div` reproductionPeriod
numChildrenOnDayN s n = ((n - s - 1) `div` reproductionPeriod) + 1

numChildrenAfterNDays :: Int -> Fish -> Int
-- numChildrenOnDayN s n = (n - s - 1 + reproductionPeriod) `div` reproductionPeriod
numChildrenAfterNDays n s = ((n - s - 1) `div` reproductionPeriod) + 1

populationOnDayN :: FishSchool -> Int -> Int
populationOnDayN fs n = L.sum $ (\f -> numChildrenOnDayN f n) <$> fs

-- populationOnDayN fs n = L.length $ dayNSchool (L.reverse fs) n

calculateFirstResult :: FilePath -> IO Text
calculateFirstResult =
  calculateResult parseSchool (\fss -> populationOnDayN (L'.head fss) 80)
