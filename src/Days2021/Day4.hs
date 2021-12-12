{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Days2021.Day4 where

import qualified Data.List.Split as S
import Import
import RIO.List ((\\))
import qualified RIO.List as L
import RIO.List.Partial ((!!))
import qualified RIO.List.Partial as L'
import RIO.Partial (read)
import qualified RIO.Text as T

type DrawnNummber = Int

type Board = [[Int]]

data Game = Game
  { gameDrawnNumbers :: [DrawnNummber],
    gameBoards :: [Board]
  }
  deriving (Eq, Show)

boardSide :: Int
boardSide = 5

getRow :: Board -> Int -> [Int]
getRow b j = b !! j

getColumn :: Board -> Int -> [Int]
getColumn b i =
  (!! i) . (b !!) <$> range
  where
    range = L'.init [0 .. boardSide]

parseFigure :: String -> Int
parseFigure = read

parseCalledNumbers :: String -> [Int]
parseCalledNumbers s = parseFigure . T.unpack <$> T.split (== ',') (T.pack s)

parseBoard :: String -> Board
parseBoard s =
  ( \stringRow ->
      parseFigure . T.unpack <$> stringRow
  )
    <$> stringRows
  where
    filterEmptyText = L.filter (\token -> token /= " " && token /= "")
    stringRows =
      filterEmptyText . T.split (== ' ')
        <$> T.lines (T.pack s)

parseInput :: String -> IO ([Int], [Board])
parseInput s = do
  let textLines = T.lines (T.pack s)
  let calledNumbers = parseCalledNumbers $ T.unpack $ L'.head textLines
  let boardTextLines = S.chunksOf 5 (L.filter (/= "") $ L'.tail textLines)
  let textBoards = T.unpack . T.intercalate "\n" <$> boardTextLines
  let boards = parseBoard <$> textBoards
  pure (calledNumbers, boards)

isWinningLine :: [DrawnNummber] -> [Int] -> Bool
isWinningLine ns = L.all (`L.elem` ns)

isWinningBoard :: [Int] -> Board -> Bool
isWinningBoard ns b =
  L.any (isWinningLine ns) (rows ++ cols)
  where
    range = L'.init [0 .. boardSide]
    rows = getRow b <$> range
    cols = getColumn b <$> range

getScore :: [Int] -> Board -> Int
getScore ns b =
  L.sum unmarkedNums * lastCalledNum
  where
    unmarkedNums =
      L.filter
        (\boardNum -> not $ L.elem boardNum ns)
        (concat b)
    lastCalledNum = L'.head ns

getWinningSequence :: [Board] -> [Int] -> ([(Board, Int)], [Int]) -> [(Board, Int)]
getWinningSequence _ [] (winners, _) = winners
getWinningSequence playing (n : ns) (winners, drawn) =
  getWinningSequence newPlaying ns (newWinners, newDrawn)
  where
    newDrawn = n : drawn
    stepWinners = L.filter (isWinningBoard newDrawn) playing
    newWinners = winners ++ ((\w -> (w, getScore newDrawn w)) <$> stepWinners)
    newPlaying = playing \\ stepWinners

calculateFirstResult :: FilePath -> IO Text
calculateFirstResult filePath = do
  fileContents <- readFileUtf8 filePath
  (calledNumbers, boards) <- (parseInput . T.unpack) fileContents
  case getWinningSequence boards calledNumbers ([], []) of
    [] -> pure "No solution!"
    ((_, score) : _) -> pure $ (T.pack . show) score

calculateSecondResult :: FilePath -> IO Text
calculateSecondResult filePath = do
  fileContents <- readFileUtf8 filePath
  (calledNumbers, boards) <- (parseInput . T.unpack) fileContents
  case reverse $ getWinningSequence boards calledNumbers ([], []) of
    [] -> pure "No solution!"
    ((_, score) : _) -> pure $ (T.pack . show) score