{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Days2021.Day11 where

import Import
import qualified RIO.List as L
import RIO.List.Partial ((!!))
import qualified RIO.Partial as P
import qualified RIO.Set as S
import qualified RIO.Text as T

data Octopus = Level Int | Flash
  deriving (Eq, Show)

type Row = [Octopus]

type Grid = [Row]

type Point = (Int, Int)

gridSize :: Int
gridSize = 10

parseOctopus :: String -> Octopus
parseOctopus = Level . P.read

parseRow :: Text -> Row
parseRow t = parseOctopus . (: "") <$> T.unpack t

parseGrid :: Text -> Grid
parseGrid t =
  if numRowsIsCorrect && numColsIsCorrect
    then parsedRows
    else error "Incorrect grid size"
  where
    parsedRows = parseRow <$> T.lines t
    numRowsIsCorrect = L.length parsedRows == gridSize
    numColsIsCorrect = L.all ((== gridSize) . L.length) parsedRows

gridToString :: Grid -> String
gridToString g = L.unlines $ rowToString <$> g
  where
    octopusToString o = case o of
      Level n -> show n
      Flash -> "F"
    rowToString =
      L.foldl (\rs o -> rs ++ octopusToString o) ""

inBounds :: Point -> Bool
inBounds (i, j) = 0 <= i && i < gridSize && 0 <= j && j < gridSize

valueAt :: Point -> Grid -> Maybe Octopus
valueAt (i, j) g =
  if inBounds (i, j)
    then Just $ (g !! j) !! i
    else Nothing

neighbours :: Point -> Grid -> Set Point
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

increaseLevel :: Octopus -> Octopus
increaseLevel (Level n) = Level (n + 1)
increaseLevel Flash = Flash

increaseLevelAt :: Point -> Grid -> Grid
increaseLevelAt (x, y) g =
  [ [ if x == i && y == j
        then increaseLevel o
        else o
      | (i, o) <- L.zip [0 ..] r
    ]
    | (j, r) <- L.zip [0 ..] g
  ]

flash :: Octopus -> Octopus
flash (Level n) =
  if n > 9
    then Flash
    else Level n
flash Flash = Flash

flashPoints :: Grid -> Set Point
flashPoints g =
  S.unions $ rowFlashPoints <$> L.zip [0 ..] g
  where
    indexedRowFlashes r = L.filter (\(_, o) -> o == Flash) (L.zip [0 ..] r)
    rowFlashPoints (j, r) = S.fromList $ (\(i, _) -> (i, j)) <$> indexedRowFlashes r

resetLevel :: Octopus -> Octopus
resetLevel Flash = Level 0
resetLevel (Level n) =
  if n > 9
    then Level 0
    else Level n

numFlashes :: Grid -> Int
numFlashes g =
  L.sum (rowNumFlashes <$> g)
  where
    rowNumFlashes = L.length . L.filter (== Flash)

addFlashEffects :: Grid -> Grid
addFlashEffects g =
  if numFlashes g == numFlashes increasedFlashed
    then increasedFlashed
    else addFlashEffects increasedFlashed
  where
    toIncrease =
      S.foldl
        (\acc flashedPoint -> S.union acc $ neighbours flashedPoint g)
        S.empty
        (flashPoints g)
    increased = L.foldl (flip increaseLevelAt) g $ S.toList toIncrease
    increasedFlashed = (flash <$>) <$> increased

nextStep :: Grid -> Grid
nextStep g =
  (resetLevel <$>) <$> addFlashEffects flashed
  where
    increasedLevels = (increaseLevel <$>) <$> g
    flashed = (flash <$>) <$> increasedLevels

evolve :: Grid -> Int -> Grid
evolve g 0 = g
evolve g n = evolve (nextStep g) (n - 1)