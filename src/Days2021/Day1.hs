module Days2021.Day1 where

import qualified RIO.List as L

compareInitialPair :: [Int] -> Int
compareInitialPair [] = 0
compareInitialPair [_] = 0
compareInitialPair (x0 : x1 : _) =
  if x0 < x1
    then 1
    else 0

countIncreases :: [Int] -> Int
countIncreases xs =
  L.sum $ L.map compareInitialPair (L.tails xs)