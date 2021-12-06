{-# LANGUAGE OverloadedStrings #-}

module Days2021.Day4 where

import Import
import qualified RIO.List as L
import qualified RIO.Prelude as P
import qualified RIO.Text as T
import Text.Pretty.Simple (pPrint)

type CalledNum = Int

type Board = [[Int]]

data Game = Game
  { gameCalledNumbers :: [Int],
    gameBoards :: [Board]
  }

parseDigit :: Text -> Either String Int
parseDigit t = case (P.readMaybe . P.show) t :: Maybe Int of
  Just n -> Right n
  Nothing -> Left ("Could not parse: " ++ P.show t)

parseBoard :: Text -> Board
parseBoard t =
  ( \l ->
      -- error $ show l
      ( \d -> case parseDigit d of
          Right n -> n
          Left errorString -> error errorString
      )
        <$> l
  )
    <$> ls
  where
    ls = L.filter (\s -> s /= " " && s /= "") . T.split (== ' ') <$> T.lines t