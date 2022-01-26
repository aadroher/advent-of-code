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

parseLine :: [Chunk] -> [Chunk] -> Either (Chunk, Chunk) ()
parseLine (Chunk Opening bt : cs) stack = parseLine cs (Chunk Opening bt : stack)
parseLine (Chunk Closing bt : cs) (s : stack) =
  if s == Chunk Opening bt
    then parseLine cs stack
    else Left (Chunk Opening bt, s)
parseLine [] [] = Right ()
parseLine _ _ = error "Could not parse!"
