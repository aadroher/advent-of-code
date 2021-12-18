{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Days2021.Day6 where

import Import
import qualified RIO.List as L

type Fish = Int

type FishSchool = [Fish]

shouldSpawn :: Fish -> Bool
shouldSpawn f = f == 6

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
      existingFishes
  where
    existingFishes = nextDayFish <$> fs

dayNSchool :: FishSchool -> Int -> FishSchool
dayNSchool fs 0 = fs
dayNSchool fs n = dayNSchool (nextDaySchool fs) (n - 1)
