{-# LANGUAGE OverloadedStrings #-}

module Days.Day3 where

import RIO
import qualified RIO.List as L
import qualified RIO.List.Partial as LP

data Direction = L | R | U | D

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

getDistToClosestIntersection :: [Instruction] -> [Instruction] -> Int
getDistToClosestIntersection is0 is1 =
  LP.minimum $
    distance (0, 0)
      <$> [p | p <- L.intersect route0 route1, p /= (0, 0)]
  where
    route0 = getRouteFromCenter is0
    route1 = getRouteFromCenter is1
