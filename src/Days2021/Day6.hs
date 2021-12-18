{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Days2021.Day6 where

import Import
import qualified RIO.List as L

newtype Fish = Fish
  { daysToSpawn :: Int
  }
  deriving (Eq, Show)

type FishSchool = [Fish]

intToFish :: Int -> Fish
intToFish n = Fish {daysToSpawn = n}

fishToInt :: Fish -> Int
fishToInt Fish {daysToSpawn = n} = n

shouldSpawn :: Fish -> Bool
shouldSpawn f = daysToSpawn f == 0

nextDayFish :: Fish -> Fish
nextDayFish f =
  if daysToSpawn f == 0
    then Fish {daysToSpawn = 0}
    else Fish {daysToSpawn = daysToSpawn f - 1}

nextDaySchool :: FishSchool -> FishSchool
nextDaySchool fs =
  existingFishes
    ++ L.foldl
      ( \newFishes f ->
          if shouldSpawn f
            then newFishes ++ [Fish {daysToSpawn = 8}]
            else newFishes
      )
      []
      existingFishes
  where
    existingFishes = nextDayFish <$> fs

dayNSchool :: FishSchool -> Int -> FishSchool
dayNSchool fs 0 = fs
dayNSchool fs n = dayNSchool (nextDaySchool fs) (n - 1)
