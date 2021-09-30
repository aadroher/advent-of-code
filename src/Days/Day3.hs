{-# LANGUAGE OverloadedStrings #-}

module Days.Day3 where

import Data.List.Split (splitOn)
import RIO
import qualified RIO.List as L
import qualified RIO.List.Partial as L'
import qualified RIO.Text as T
import qualified RIO.Text.Partial as T'

data Direction = L | R | U | D
  deriving (Show, Eq)

type Instruction = (Direction, Int)

type Position = (Int, Int)

getInstructionSteps :: Instruction -> Position -> [Position]
getInstructionSteps (L, d) (x, y) = [(x - s, y) | s <- [1 .. d]]
getInstructionSteps (R, d) (x, y) = [(x + s, y) | s <- [1 .. d]]
getInstructionSteps (U, d) (x, y) = [(x, y + s) | s <- [1 .. d]]
getInstructionSteps (D, d) (x, y) = [(x, y - s) | s <- [1 .. d]]

getRoute :: [Position] -> [Instruction] -> [Position]
getRoute =
  foldl'
    (\steps instruction -> steps ++ getInstructionSteps instruction (last steps))

getRouteFromCenter :: [Instruction] -> [Position]
getRouteFromCenter = getRoute [(0, 0)]

distance :: Position -> Position -> Int
distance (x0, y0) (x1, y1) = (x1 - x0) + (y1 - y0)

parseDirection :: Text -> Direction
parseDirection s = case s of
  "L" -> L
  "R" -> R
  "U" -> U
  "D" -> D
  _ -> error "Could not parse Direction"

parseInstruction :: Text -> Instruction
parseInstruction s = case T'.head s of
  dirStr -> ((parseDirection . fromString) [dirStr], read $ T.unpack (T'.tail s))

parseInstructions :: Text -> [Instruction]
parseInstructions s = parseInstruction <$> T'.splitOn "," s

getDistToClosestIntersection :: [Instruction] -> [Instruction] -> Int
getDistToClosestIntersection is0 is1 =
  L'.minimum $
    distance (0, 0)
      <$> [p | p <- L.intersect route0 route1, p /= (0, 0)]
  where
    route0 = getRouteFromCenter is0
    route1 = getRouteFromCenter is1
