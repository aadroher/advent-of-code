{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Days2021.Day5 where

import Import
import RIO.HashMap (HashMap)
import qualified RIO.HashMap as HM
import qualified RIO.List as L
import RIO.Partial (read)
import qualified RIO.Text as T

type Point = (Int, Int)

type Line = (Point, Point)

parseFigure :: Text -> Int
parseFigure = read . T.unpack

parsePoint :: Text -> Point
parsePoint s = case T.split (== ',') s of
  [x, y] -> (parseFigure x, parseFigure y)
  _ -> error ("could not parse '" ++ T.unpack s ++ "' as Point")

parseLine :: Text -> Line
parseLine s = case T.split (== ' ') s of
  [p0, _, p1] -> (parsePoint p0, parsePoint p1)
  _ -> error ("could not parse '" ++ T.unpack s ++ "' as Line")

expandLinePoints :: Line -> [Point]
expandLinePoints ((x0, y0), (x1, y1)) =
  [ (x, y) | x <- [x0, (secondElem x0 x1) .. x1], y <- [y0, (secondElem y0 y1) .. y1]
  ]
  where
    secondElem z0 z1 = bool (z0 + 1) (z0 - 1) (z1 < z0)

isOrthogonal :: Line -> Bool
isOrthogonal ((x0, y0), (x1, y1)) = x0 == x1 || y0 == y1

getLinePointsCount :: Line -> HashMap Point Int
getLinePointsCount l =
  HM.fromListWith
    (+)
    [ (p, 1) | p <- expandLinePoints l
    ]

getTotalPointsCount :: [Line] -> HashMap Point Int
getTotalPointsCount =
  L.foldl
    (\hm l -> HM.unionWith (+) hm (getLinePointsCount l))
    HM.empty

countOverlappingPoints :: [Line] -> Int
countOverlappingPoints ls =
  HM.size $ HM.filter (== 2) $ getTotalPointsCount ls

countOrthogonalOverlappingPoints :: [Line] -> Int
countOrthogonalOverlappingPoints ls =
  countOverlappingPoints $ L.filter isOrthogonal ls