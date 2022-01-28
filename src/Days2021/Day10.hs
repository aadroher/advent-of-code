{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Days2021.Day10 where

import Import
import qualified RIO.List as L
import RIO.List.Partial ((!!))
import qualified RIO.Text as T
import Util (calculateResult)

data BrackeType = Round | Square | Curly | Angle
  deriving (Eq, Show)

data Position = Opening | Closing
  deriving (Eq, Show)

data Chunk = Chunk Position BrackeType
  deriving (Eq, Show)

data ParseResult = Corrupt (Chunk, Maybe Chunk) | Incomplete [Chunk] | Ok
  deriving (Eq, Show)

invert :: Chunk -> Chunk
invert (Chunk Opening bt) = Chunk Closing bt
invert (Chunk Closing bt) = Chunk Opening bt

lexChunk :: Char -> Chunk
lexChunk c = case c of
  '(' -> Chunk Opening Round
  '[' -> Chunk Opening Square
  '{' -> Chunk Opening Curly
  '<' -> Chunk Opening Angle
  ')' -> Chunk Closing Round
  ']' -> Chunk Closing Square
  '}' -> Chunk Closing Curly
  '>' -> Chunk Closing Angle
  _ -> error $ "Could not parse char: " ++ [c]

lexLine :: Text -> [Chunk]
lexLine t = lexChunk <$> T.unpack t

bracketType :: Chunk -> BrackeType
bracketType (Chunk _ bt) = bt

parseLine :: [Chunk] -> [Chunk] -> ParseResult
parseLine (c : cs) [] = case c of
  Chunk Opening bt -> parseLine cs [Chunk Opening bt]
  _ -> Corrupt (c, Nothing)
parseLine (c : cs) (sc : scs) = case c of
  Chunk Opening bt -> parseLine cs (Chunk Opening bt : sc : scs)
  Chunk Closing bt ->
    if sc == Chunk Opening bt
      then parseLine cs scs
      else Corrupt (c, Just $ Chunk Closing $ bracketType sc)
parseLine [] (sc : scs) = Incomplete (sc : scs)
parseLine [] [] = Ok

mismatchScore :: ParseResult -> Int
mismatchScore (Corrupt (Chunk Closing bt, _)) = case bt of
  Round -> 3
  Square -> 57
  Curly -> 1197
  Angle -> 25137
mismatchScore _ = 0

sumMismatchScores :: [ParseResult] -> Int
sumMismatchScores mms = sum $ mismatchScore <$> mms

completeSequence :: [Chunk] -> [Chunk]
completeSequence cs = case parseLine cs [] of
  Incomplete scs -> invert <$> scs
  _ -> []

completionScore :: ParseResult -> Int
completionScore (Incomplete scs) =
  L.foldl (\n c -> 5 * n + chunkScore c) 0 scs
  where
    chunkScore c = case bracketType c of
      Round -> 1
      Square -> 2
      Curly -> 3
      Angle -> 4
completionScore _ = 0

completionWinningScore :: [ParseResult] -> Int
completionWinningScore prs =
  L.sort scores !! (L.length scores `div` 2)
  where
    scores =
      [ completionScore pr
        | pr <- prs,
          completionScore pr > 0
      ]

calculateFirstResult :: FilePath -> IO Text
calculateFirstResult =
  calculateResult
    lexLine
    (\css -> sumMismatchScores $ (`parseLine` []) <$> css)

calculateSecondResult :: FilePath -> IO Text
calculateSecondResult =
  calculateResult
    lexLine
    (\css -> completionWinningScore $ (`parseLine` []) <$> css)
