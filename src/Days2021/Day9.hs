{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Days2021.Day9 where

import Import
import qualified RIO.List as L
import RIO.List.Partial ((!!))
import RIO.List.Partial as L'
import qualified RIO.Partial as P
import qualified RIO.Set as S
import qualified RIO.Text as T
import Util (calculateResult)

type FloorMap = [[Int]]

type Point = (Int, Int)

parseRow :: Text -> [Int]
parseRow t = P.read . (: "") <$> T.unpack t

isValid :: FloorMap -> Bool
isValid [] = True
isValid [_] = True
isValid (r0 : r1 : rs) = L.length r0 == L.length r1 && isValid (r1 : rs)

dimensions :: FloorMap -> Maybe (Int, Int)
dimensions fm =
  if isValid fm
    then
      ( case L.headMaybe fm of
          Just r -> Just (L.length r, L.length fm)
          Nothing -> Nothing
      )
    else Nothing

inBounds :: Point -> FloorMap -> Bool
inBounds (i, j) fm = case dimensions fm of
  Just (n, m) -> 0 <= i && i < n && 0 <= j && j < m
  Nothing -> False

valueAt :: Point -> FloorMap -> Maybe Int
valueAt (i, j) fm =
  if inBounds (i, j) fm
    then Just $ (fm !! j) !! i
    else Nothing

neighbours :: Point -> FloorMap -> Set Point
neighbours (i, j) fm =
  S.fromList $
    L.foldl
      ( \ps p ->
          case valueAt p fm of
            Just _ -> p : ps
            Nothing -> ps
      )
      []
      offsets
  where
    offsets =
      [ (i, j + 1),
        (i, j - 1),
        (i + 1, j),
        (i + 1, j + 1),
        (i + 1, j - 1),
        (i - 1, j),
        (i - 1, j + 1),
        (i - 1, j - 1)
      ]

neighbourValues :: Point -> FloorMap -> [Int]
neighbourValues p fm =
  L.foldl
    ( \nvs n ->
        case valueAt n fm of
          Just nv -> nv : nvs
          Nothing -> nvs
    )
    []
    (neighbours p fm)

isLowPoint :: Point -> FloorMap -> Bool
isLowPoint p fm =
  case valueAt p fm of
    Just pv -> L.all (>= pv) $ neighbourValues p fm
    Nothing -> False

lowPoints :: FloorMap -> Set Point
lowPoints fm =
  case dimensions fm of
    Nothing -> S.empty
    Just (n, m) ->
      S.fromList $
        L.filter
          (`isLowPoint` fm)
          [ (i, j)
            | i <- [0 .. (n - 1)],
              j <- [0 .. (m - 1)]
          ]

risk :: Point -> FloorMap -> Maybe Int
risk p fm = (+ 1) <$> valueAt p fm

sumRiskLevels :: FloorMap -> Int
sumRiskLevels fm =
  L.foldl
    ( \s r -> case r of
        Just x -> s + x
        Nothing -> s
    )
    0
    risks
  where
    risks = (`risk` fm) <$> S.toList (lowPoints fm)

nextStep :: FloorMap -> Point -> Point
nextStep fm p =
  S.foldl
    ( \lowest n ->
        if valueAt n fm < valueAt lowest fm
          then n
          else lowest
    )
    initial
    (S.fromList ns)
  where
    (initial : ns) = S.toList $ neighbours p fm

extendPathToLowPoint :: FloorMap -> [Point] -> [Point]
extendPathToLowPoint _ [] = []
extendPathToLowPoint fm ps@(p : _) =
  if isLowPoint p fm
    then ps
    else extendPathToLowPoint fm (minP : ps)
  where
    minP = nextStep fm p

shifts :: [(Int, Int)]
shifts =
  [ (0, 1),
    (0, -1),
    (1, 0),
    (-1, 0)
  ]

addStep :: Set Point -> Point -> Set Point
addStep ps p@(di, dj) = case S.toList ps of
  [] -> S.singleton p
  psl ->
    let (i, j) = L'.maximum psl
     in S.insert (i + di, j + dj) ps

pathsOf :: Int -> Point -> FloorMap -> Set (Set Point)
pathsOf 0 p _ = S.fromList [S.singleton p]
pathsOf n p fm =
  S.foldl
    ( \newPaths pathsOfMinus1 ->
        S.union newPaths $
          S.fromList $
            ( (\ds -> addStep pathsOfMinus1 ds)
                <$> shifts
            )
    )
    S.empty
    (pathsOf (n -1) p fm)

radiusOfAt :: Int -> Point -> FloorMap -> Set Point
radiusOfAt 0 _ _ = S.empty
radiusOfAt n (x, y) fm = S.unions $ S.filter (`inBounds` fm) <$> [diagA, diagB, diagC, diagD]
  where
    diagA =
      S.fromList
        [ (x + i, y - j)
          | (i, j) <- L.zip [0 .. (n - 1)] [n, (n - 1) .. 1]
        ]
    diagB =
      S.fromList
        [ (x + i, y + j)
          | (i, j) <- L.zip [n, (n - 1) .. 1] [0 .. (n - 1)]
        ]
    diagC =
      S.fromList
        [ (x - i, y + j)
          | (i, j) <- L.zip [0 .. n] [n, (n - 1) .. 1]
        ]
    diagD =
      S.fromList
        [ (x - i, y - j)
          | (i, j) <- L.zip [n, (n - 1) .. 1] [0 .. (n - 1)]
        ]

basinAt :: Point -> FloorMap -> Set Point
basinAt p fm = S.empty

calculateFirstResult :: FilePath -> IO Text
calculateFirstResult = calculateResult parseRow sumRiskLevels