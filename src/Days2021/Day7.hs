{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Days2021.Day7 where

import Import
import qualified RIO.List as L
import qualified RIO.List.Partial as L'
import RIO.Partial (read)
import qualified RIO.Text as T
import Util (calculateResult)

type Position = Int

type Cost = Int

type Alignment = (Position, Cost)

parsePositions :: Text -> [Position]
parsePositions t = read . T.unpack <$> T.split (== ',') t

getRange :: [Position] -> [Int]
getRange ps = [L'.minimum ps .. L'.maximum ps]

getTotalCost :: [Position] -> Position -> Cost
getTotalCost ps target = L.sum $ (\p -> abs (target - p)) <$> ps

triangularNum :: Int -> Int
triangularNum n = L.sum [0 .. n]

getWeightedTotalCost :: [Position] -> Position -> Cost
getWeightedTotalCost ps target = L.sum $ (\p -> triangularNum $ abs (target - p)) <$> ps

optimalAlignment :: ([Position] -> Position -> Cost) -> [Position] -> Alignment
optimalAlignment calculateCost ps =
  case L.find ((== minCost) . snd) costs of
    Just a -> a
    Nothing -> error "Could not find alignment."
  where
    range = getRange ps
    costs = (\target -> (target, calculateCost ps target)) <$> range
    minCost = L'.minimum $ snd <$> costs

getNaiveOptimalAlignment :: [Position] -> Alignment
getNaiveOptimalAlignment = optimalAlignment getTotalCost

getWeightedOptimalAlignment :: [Position] -> Alignment
getWeightedOptimalAlignment = optimalAlignment getWeightedTotalCost

calculateFirstResult :: FilePath -> IO Text
calculateFirstResult = calculateResult parsePositions (getNaiveOptimalAlignment . L'.head)

calculateSecondResult :: FilePath -> IO Text
calculateSecondResult = calculateResult parsePositions (getWeightedOptimalAlignment . L'.head)