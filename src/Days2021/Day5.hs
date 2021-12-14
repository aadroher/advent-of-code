{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Days2021.Day5 where

import Import
import RIO.HashMap (HashMap)
import qualified RIO.HashMap as HM
import qualified RIO.List as L
import RIO.Partial (read)
import qualified RIO.Text as T
import Util (calculateResult)

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

expandOrthogonalLinePoints :: Line -> [Point]
expandOrthogonalLinePoints ((x0, y0), (x1, y1)) =
  case (x0 == x1, y0 == y1) of
    (False, False) -> error "Not an orthogonal line"
    (True, False) -> [(x0, y) | y <- [y0, (secondElem y0 y1) .. y1]]
    (False, True) -> [(x, y0) | x <- [x0, (secondElem x0 x1) .. x1]]
    (True, True) -> [(x0, y0)]
  where
    secondElem z0 z1 = bool (z0 + 1) (z0 - 1) (z1 < z0)

expandDiagonalLinePoints :: Line -> [Point]
expandDiagonalLinePoints ((x0, y0), (x1, y1)) =
  L.zip
    [x0, (secondElem x0 x1) .. x1]
    [y0, (secondElem y0 y1) .. y1]
  where
    secondElem z0 z1 = bool (z0 + 1) (z0 - 1) (z1 < z0)

expandLinePoints :: Line -> [Point]
expandLinePoints l
  | isOrthogonal l = expandOrthogonalLinePoints l
  | isDiagonal l = expandOrthogonalLinePoints l
  | otherwise = error "Not orthogonal or diagonal"

isOrthogonal :: Line -> Bool
isOrthogonal ((x0, y0), (x1, y1)) = x0 == x1 || y0 == y1

isDiagonal :: Line -> Bool
isDiagonal ((x0, y0), (x1, y1)) = x1 - x0 == y1 - y0

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

countOverlappingPoints :: (Int -> Bool) -> (Line -> Bool) -> [Line] -> Int
countOverlappingPoints countOnly linesFilter ls =
  HM.size $ HM.filter countOnly $ getTotalPointsCount filteredLines
  where
    filteredLines = L.filter linesFilter ls

countOverlappingPointsUpTo2 :: (Line -> Bool) -> [Line] -> Int
countOverlappingPointsUpTo2 = countOverlappingPoints (>= 2)

countOrthogonalOverlappingPoints :: [Line] -> Int
countOrthogonalOverlappingPoints ls =
  countOverlappingPointsUpTo2 isOrthogonal $ L.filter isOrthogonal ls

calculateFirstResult :: FilePath -> IO Text
calculateFirstResult =
  calculateResult parseLine countOrthogonalOverlappingPoints