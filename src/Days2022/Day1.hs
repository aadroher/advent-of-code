{-# LANGUAGE OverloadedStrings #-}

module Days2022.Day1 where

import Import
import qualified RIO.List as L
import qualified RIO.List.Partial as L'
import qualified RIO.Text as T
import Util (calculateResult)

parseInput :: [String] -> [[Int]]
parseInput = L.foldl f []
  where
    f [] "" = []
    f [] s = [[read s]]
    f batches "" = [] : batches
    f (batch : batches) s = (read s : batch) : batches

getMaxCalorieCount :: [[Int]] -> Int
getMaxCalorieCount batches = L'.maximum $ L.sum <$> batches

getTop3CalorieCountSum :: [[Int]] -> Int
getTop3CalorieCountSum batches = L.sum $ L.take 3 $ L.reverse $ L.sort $ L.sum <$> batches

calculateFirstResult :: FilePath -> IO Text
calculateFirstResult = calculateResult T.unpack (getMaxCalorieCount . parseInput)

calculateSecondResult :: FilePath -> IO Text
calculateSecondResult = calculateResult T.unpack (getTop3CalorieCountSum . parseInput)