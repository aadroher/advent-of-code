{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Days2021.Day7 where

import Import
import qualified RIO.List as L
import qualified RIO.List.Partial as L'

type Position = Int

type Cost = Int

type Alignment = (Position, Cost)

getRange :: [Position] -> [Int]
getRange ps = [L'.minimum ps .. L'.maximum ps]

getTotalCost :: [Position] -> Position -> Cost
getTotalCost ps target = L.sum $ (\p -> abs (target - p)) <$> ps

optimalAlignment :: [Position] -> Maybe Alignment
optimalAlignment ps =
  L.find ((== minCost) . snd) costs
  where
    range = getRange ps
    costs = (\target -> (target, getTotalCost ps target)) <$> range
    minCost = L'.minimum $ snd <$> costs
