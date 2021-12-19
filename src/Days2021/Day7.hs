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

optimalAlignment :: [Position] -> Alignment
optimalAlignment ps =
  case L.find ((== minCost) . snd) costs of
    Just a -> a
    Nothing -> error "Could not find alignment."
  where
    range = getRange ps
    costs = (\target -> (target, getTotalCost ps target)) <$> range
    minCost = L'.minimum $ snd <$> costs

calculateFirstResult :: FilePath -> IO Text
calculateFirstResult = calculateResult parsePositions (optimalAlignment . L'.head)