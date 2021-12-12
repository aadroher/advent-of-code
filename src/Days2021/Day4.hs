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
import qualified RIO.Prelude as P
import qualified RIO.Text as T
import qualified RIO.Text.Partial as T'
import Text.Pretty.Simple (pPrint)

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

isWinningLine :: [DrawnNummber] -> [Int] -> Bool
isWinningLine ns = L.all (`L.elem` ns)

isWinningBoard :: [Int] -> Board -> Bool
isWinningBoard ns b =
  L.any (isWinningLine ns) (rows ++ cols)
  where
    range = L'.init [0 .. boardSide]
    rows = getRow b <$> range
    cols = getColumn b <$> range

getScore :: [DrawnNummber] -> Board -> Int
getScore ns b =
  L.sum unmarkedNums * lastCalledNum
  where
    unmarkedNums =
      L.filter
        (\boardNum -> not $ L.elem boardNum ns)
        (concat b)
    lastCalledNum = L'.last ns

getWinningBoard :: Game -> Maybe Board
getWinningBoard g = L.find (isWinningBoard $ gameDrawnNumbers g) (gameBoards g)

getWinningBoards :: Game -> [Board]
getWinningBoards g = L.filter (isWinningBoard $ gameDrawnNumbers g) (gameBoards g)

callNumbers :: [Board] -> [Int] -> [Int] -> [Board] -> [Board]
callNumbers _ [] _ winners = winners
callNumbers playing (n : ns) drawn winners =
  callNumbers newPlaying ns newDrawn newWinners
  where
    newDrawn = n : drawn
    newWinners = L.filter (isWinningBoard newDrawn) playing ++ winners
    newPlaying = playing \\ newWinners

-- getWinningSequence :: Game -> [Board]
-- getWinningSequence g = foldl (b -> a -> b) [] $ gameDrawnNumbers g
--   where
--     addWinningBoards =
--       (\winningSequence n ->
--         let new
--       )

play :: Game -> [Int] -> Game
play g [] = g
play g ns = case getWinningBoard g of
  Just _ -> g
  Nothing -> play newGame (L'.tail ns)
  where
    newDrawnNumbers = gameDrawnNumbers g ++ [L'.head ns]
    newGame =
      Game
        { gameDrawnNumbers = newDrawnNumbers,
          gameBoards = gameBoards g
        }

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

calculateFirstResult :: FilePath -> IO Text
calculateFirstResult filePath = do
  fileContents <- readFileUtf8 filePath
  (calledNumbers, boards) <- (parseInput . T.unpack) fileContents
  let initialGame =
        Game
          { gameDrawnNumbers = [],
            gameBoards = boards
          }
  let endGame = play initialGame calledNumbers
  let sequenceToWinningMove = gameDrawnNumbers endGame
  case getWinningBoard endGame of
    Nothing -> pure "No solution!"
    Just b -> pure $ (T.pack . show) $ getScore sequenceToWinningMove b