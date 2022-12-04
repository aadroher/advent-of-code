{-# LANGUAGE OverloadedStrings #-}

module Days2022.Day1 where

import Import
import qualified RIO.List as L
import qualified RIO.List.Partial as L'
import qualified RIO.Text as T
import Util (calculateResult)

getMaxCalorieCount :: [[Int]] -> Int
getMaxCalorieCount batches = L'.maximum $ L.sum <$> batches

-- parseLine :: String -> Maybe Int
-- parseLine s =
--   case readMaybe s of
--     Nothing ->

parseInput :: [String] -> [[Int]]
parseInput = L.foldl f []
  where
    f [] "" = []
    f [] s = [[read s]]
    f batches "" = [] : batches
    f (batch : batches) s = (read s : batch) : batches

calculateFirstResult :: FilePath -> IO Text
calculateFirstResult = calculateResult (T.unpack) (getMaxCalorieCount . parseInput)