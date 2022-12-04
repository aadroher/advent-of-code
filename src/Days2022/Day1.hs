{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Days2022.Day1 where

import Import
import qualified RIO.List as L
import qualified RIO.List.Partial as L'

getMaxCalorieCount :: [[Int]] -> Int
getMaxCalorieCount batches = L'.maximum $ L.sum <$> batches