{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Days2021.Day9 where

import Import
import qualified RIO.List as L
import RIO.List.Partial ((!!))
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

isLowPoint :: Point -> FloorMap -> Bool
isLowPoint p fm =
  case valueAt p fm of
    Just pv -> L.all (> pv) neighbourValues
    Nothing -> False
  where
    neighbourValues =
      L.foldl
        ( \nvs n ->
            case valueAt n fm of
              Just nv -> nv : nvs
              Nothing -> nvs
        )
        []
        (neighbours p fm)

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

calculateFirstResult :: FilePath -> IO Text
calculateFirstResult = calculateResult parseRow sumRiskLevels