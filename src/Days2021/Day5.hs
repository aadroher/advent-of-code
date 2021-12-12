{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Days2021.Day5 where

import Import
import qualified RIO.List as L

type Point = (Int, Int)

type Line = (Point, Point)

expandLinePoints :: Line -> [Point]
expandLinePoints ((x0, y0), (x1, y1)) =
  [ (x, y) | x <- [x0, (secondElem x0 x1) .. x1], y <- [y0, (secondElem y0 y1) .. y1]
  ]
  where
    secondElem z0 z1 = bool (z0 + 1) (z0 - 1) (z1 < z0)
