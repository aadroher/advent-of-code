{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Days2021.Day4 where

import Import
import qualified RIO.List as L
import RIO.List.Partial ((!!))
import qualified RIO.List.Partial as L'
import qualified RIO.Prelude as P
import qualified RIO.Text as T
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
getScore = undefined

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

-- parseFigure :: Text -> Either String Int
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