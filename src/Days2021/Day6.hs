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

parseSchool :: Text -> FishSchool
parseSchool t = read . T.unpack <$> T.split (== ',') t

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
  L.foldl'
    ( \newFishes f ->
        if shouldSpawn f
          then 8 : newFishes
          else newFishes
    )
    []
    fs
    ++ existingFishes
  where
    existingFishes = nextDayFish <$> fs

firstNDays :: Int -> FishSchool -> [FishSchool]
firstNDays n fs = L.take (n + 1) $ iterate' nextDaySchool fs

dayNSchool :: FishSchool -> Int -> FishSchool
dayNSchool fs n = L.foldl' (\fs' _ -> nextDaySchool fs') fs [1 .. n]

populationOnDayN :: FishSchool -> Int -> Int
populationOnDayN fs n = L.length $ dayNSchool (L.reverse fs) n

calculateFirstResult :: FilePath -> IO Text
calculateFirstResult =
  calculateResult parseSchool (\fss -> populationOnDayN (L'.head fss) 80)
