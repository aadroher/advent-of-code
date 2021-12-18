{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Days2021.Day6 where

import Import
import qualified RIO.List as L

type Fish = Int

type FishSchool = [Fish]

shouldSpawn :: Fish -> Bool
shouldSpawn 0 = True
shouldSpawn _ = False

nextDayFish :: Fish -> Fish
nextDayFish f =
  if f == 0
    then 6
    else f - 1

nextDaySchool :: FishSchool -> FishSchool
nextDaySchool fs =
  existingFishes
    ++ L.foldl
      ( \newFishes f ->
          if shouldSpawn f
            then newFishes ++ [8]
            else newFishes
      )
      []
      fs
  where
    existingFishes = nextDayFish <$> fs

firstNDays :: Int -> FishSchool -> [FishSchool]
firstNDays n fs = L.take (n + 1) $ L.iterate nextDaySchool fs

dayNSchool :: FishSchool -> Int -> FishSchool
dayNSchool fs 0 = fs
dayNSchool fs n = dayNSchool (nextDaySchool fs) (n - 1)

populationOnDayN :: FishSchool -> Int -> Int
populationOnDayN fs n = L.length $ dayNSchool fs n
