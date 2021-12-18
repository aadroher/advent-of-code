{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Days2021.Day5 where

import Import
import qualified RIO.HashMap as HM
import qualified RIO.List as L
import qualified RIO.List.Partial as L'
import RIO.Partial (read)
import qualified RIO.Text as T
import Util (calculateResult)

type Point = (Int, Int)

data DiagonalDir = UpDir | DownDir
  deriving (Eq)

type Pair = (Point, Point)

data Line
  = Ver Int (Int, Int)
  | Hor (Int, Int) Int
  | Dia (Int, Int) (Int, Int)
  -- | DiagUp (Int, Int) Int
  -- | DiagDown (Int, Int) Int
  deriving (Eq, Show)

isVertical :: Pair -> Bool
isVertical ((x0, _), (x1, _)) = x0 == x1

isHorizontal :: Pair -> Bool
isHorizontal ((_, y0), (_, y1)) = y0 == y1

isOrthogonal :: Pair -> Bool
isOrthogonal p = isVertical p || isHorizontal p

isDiagonal :: Pair -> Bool
isDiagonal ((x0, y0), (x1, y1)) = abs (x0 - x1) == abs (y0 - y1)

getDiagonalDir :: Pair -> DiagonalDir
getDiagonalDir (p0, p1) =
  if ((y1 - y0) `div` (x1 - x0)) >= 0
    then UpDir
    else DownDir
  where
    [(x0, y0), (x1, y1)] = L.sort [p0, p1]

parseFigure :: Text -> Int
parseFigure = read . T.unpack

parsePoint :: Text -> Point
parsePoint s = case T.split (== ',') s of
  [x, y] -> (parseFigure x, parseFigure y)
  _ -> error ("could not parse '" ++ T.unpack s ++ "' as Point")

pair2Line :: Pair -> Line
pair2Line p
  | isVertical p =
    let ((x, y0), (_, y1)) = p in Ver x (y0, y1)
  | isHorizontal p =
    let ((x0, y), (x1, _)) = p in Hor (x0, x1) y
  | isDiagonal p =
    let ((x0, y0), (x1, y1)) = p
     in Dia (x0, y0) (x1, y1)
  | otherwise = error "Cannot parse line"

parsePair :: Text -> Pair
parsePair s = case T.split (== ' ') s of
  [p0, _, p1] -> (parsePoint p0, parsePoint p1)
  _ -> error ("could not parse '" ++ T.unpack s ++ "' as Line")

getOrthRange :: Int -> Int -> [Int]
getOrthRange z0 zn = [z0, z1 .. zn]
  where
    z1 = bool (z0 + 1) (z0 - 1) (zn < z0)

expandLinePoints :: Line -> [Point]
expandLinePoints (Ver x (y0, y1)) = [(x, y) | y <- getOrthRange y0 y1]
expandLinePoints (Hor (x0, x1) y) = [(x, y) | x <- getOrthRange x0 x1]
expandLinePoints (Dia (x0, y0) (x1, y1)) = L.zip (getOrthRange x0 x1) (getOrthRange y0 y1)
-- expandLinePoints (DiagDown (x0, y0) n) = [(x0 - i, y0 + i) | i <- [0 .. n]]

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

countOverlappingPoints :: (Int -> Bool) -> [Line] -> Int
countOverlappingPoints countOnly ls =
  HM.size $ HM.filter countOnly $ getTotalPointsCount ls

countOverlappingPointsUpTo2 :: [Line] -> Int
countOverlappingPointsUpTo2 = countOverlappingPoints (>= 2)

countOrthogonalOverlappingPoints :: [Pair] -> Int
countOrthogonalOverlappingPoints ps =
  countOverlappingPointsUpTo2 orthogonalLines
  where
    orthogonalLines = pair2Line <$> L.filter isOrthogonal ps

renderPoint :: [Line] -> Point -> String
renderPoint ls p = maybe "." show (HM.lookup p pointsCount)
  where
    pointsCount = getTotalPointsCount ls

renderRow :: [Line] -> Int -> (Int, Int) -> String
renderRow ls y (x0, xn) =
  L.concatMap
    (\x -> renderPoint ls (x, y))
    [x0 .. xn]

renderLines :: [Line] -> String
renderLines ls =
  "\n"
    ++ L.concatMap
      ( \y ->
          renderRow ls y (minX, maxX) ++ "\n"
      )
      [minY .. maxY]
  where
    pointsCount = getTotalPointsCount ls
    points = HM.keys pointsCount
    minX = L'.minimum $ fst <$> points
    maxX = L'.maximum $ fst <$> points
    minY = L'.minimum $ snd <$> points
    maxY = L'.maximum $ snd <$> points

countAllOverlappingPoints :: [Pair] -> Int
countAllOverlappingPoints ps =
  countOverlappingPointsUpTo2 $ pair2Line <$> ps

calculateFirstResult :: FilePath -> IO Text
calculateFirstResult =
  calculateResult parsePair countOrthogonalOverlappingPoints