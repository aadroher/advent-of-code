{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Days2021.Day10 where

import Import

data BrackeType = Round | Square | Curly | Angle
  deriving (Eq, Show)

data Position = Opening | Closing
  deriving (Eq, Show)

data Chunk = Chunk Position BrackeType
  deriving (Eq, Show)

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

lexLine :: String -> [Chunk]
lexLine = fmap lexChunk

bracketType :: Chunk -> BrackeType
bracketType (Chunk _ bt) = bt

parseLine :: [Chunk] -> [Chunk] -> Either (Chunk, Maybe Chunk) ()
parseLine (c : cs) [] = case c of
  Chunk Opening bt -> parseLine cs [Chunk Opening bt]
  _ -> Left (c, Nothing)
parseLine (c : cs) (sc : scs) = case c of
  Chunk Opening bt -> parseLine cs (Chunk Opening bt : sc : scs)
  Chunk Closing bt ->
    if sc == Chunk Opening bt
      then parseLine cs scs
      else Left (c, Just $ Chunk Closing $ bracketType sc)
parseLine [] [] = Right ()
parseLine _ _ = error "Could not parse!"
