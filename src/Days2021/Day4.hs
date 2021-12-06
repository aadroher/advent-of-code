{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Days2021.Day4 where

import Import
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

isWinningBoard :: [DrawnNummber] -> Board -> Bool
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

-- parseFigure t = case (P.readMaybe . P.show) t :: Maybe Int of
--   Just n -> Right n
--   Nothing -> Left ("Could not parse: " ++ P.show t)

-- parseBoard :: Text -> Board
-- parseBoard t =
--   ( \l ->
--       -- error $ show l
--       ( \d -> case parseFigure d of
--           Right n -> n
--           Left errorString -> error errorString
--       )
--         <$> l
--   )
--     <$> ls
--   where
--     ls = L.filter (\s -> s /= " " && s /= "") . T.split (== ' ') <$> T.lines t