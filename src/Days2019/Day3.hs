{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Days2019.Day3 where

import Control.Monad as M
import Import
import qualified RIO.List as L
import qualified RIO.List.Partial as L'
import qualified RIO.Set as S
import qualified RIO.Text as T
import qualified RIO.Text.Partial as T'

data Direction = L | R | U | D
  deriving (Show, Eq)

type Instruction = (Direction, Int)

type Position = (Int, Int)

centre :: Position
centre = (0, 0)

isOpposite :: Direction -> Direction -> Bool
isOpposite L R = True
isOpposite R L = True
isOpposite U D = True
isOpposite D U = True
isOpposite _ _ = False

getInstructionSteps :: Instruction -> Position -> [Position]
getInstructionSteps (L, d) (x, y) = [(x - s, y) | s <- [1 .. d]]
getInstructionSteps (R, d) (x, y) = [(x + s, y) | s <- [1 .. d]]
getInstructionSteps (U, d) (x, y) = [(x, y + s) | s <- [1 .. d]]
getInstructionSteps (D, d) (x, y) = [(x, y - s) | s <- [1 .. d]]

getRoute :: [Position] -> [Instruction] -> [Position]
getRoute =
  L.foldl'
    (\steps instruction -> steps ++ getInstructionSteps instruction (L'.last steps))

getRouteFromCenter :: [Instruction] -> [Position]
getRouteFromCenter = getRoute [centre]

distance :: Position -> Position -> Int
distance (x0, y0) (x1, y1) = abs (x1 - x0) + abs (y1 - y0)

parseDirection :: Text -> Direction
parseDirection s = case s of
  "L" -> L
  "R" -> R
  "U" -> U
  "D" -> D
  _ -> error "Could not parse Direction"

parseInstruction :: Text -> Instruction
parseInstruction s =
  case readMaybe $ T.unpack (T'.tail s) of
    Just steps -> (dir, steps)
    Nothing -> error ("Could not read " ++ T.unpack s)
  where
    dir = (parseDirection . fromString) [T'.head s]

parseInstructions :: Text -> [Instruction]
parseInstructions s = parseInstruction <$> T'.splitOn "," s

getPerimeter :: Position -> Int -> Set Position
getPerimeter c r =
  let permsWithRep = M.replicateM r [L, R, U, D]
   in S.fromList $
        L.filter
          (\(a, b) -> (abs a + abs b) == r)
          [ L'.last (getRoute [c] [(dir, 1) | dir <- stepsDir]) | stepsDir <- permsWithRep
          ]

getRouteIntersections :: [Position] -> [Position] -> Set Position
getRouteIntersections v w = S.fromList $ L.intersect v w

getDistToClosestIntersection :: [Instruction] -> [Instruction] -> Int
getDistToClosestIntersection is0 is1 =
  fst $
    L'.head $
      L.filter
        (\(_, ps) -> S.size (S.intersection ps route0) > 0 || S.size (S.intersection ps route1) > 0)
        [(r, getPerimeter centre r) | r <- [1 ..]]
  where
    route0 = S.fromList $ getRouteFromCenter is0
    route1 = S.fromList $ getRouteFromCenter is1

loadData :: FilePath -> IO ([Instruction], [Instruction])
loadData f = do
  fileContents <- readFileUtf8 f
  let (isStr0, isStr1) = T'.breakOn "\n" fileContents
  pure (parseInstructions isStr0, parseInstructions (T'.tail isStr1))

calculateFirstResult :: FilePath -> IO Text
calculateFirstResult p = do
  (is0, is1) <- loadData p
  let res = getDistToClosestIntersection is0 is1
  pure $ (T.pack . show) res
