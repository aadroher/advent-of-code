{-# LANGUAGE OverloadedStrings #-}

module Days.Day3 where

import Data.List.Split (splitOn)
import RIO
import qualified RIO.List as L
import qualified RIO.Set as Set
import qualified RIO.Text as T
import qualified RIO.Text.Partial as T'
import Text.Pretty.Simple (pPrint)

data Direction = L | R | U | D
  deriving (Show, Eq)

type Instruction = (Direction, Int)

type Position = (Int, Int)

centre :: Position
centre = (0, 0)

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
getRouteFromCenter = getRoute [centre]

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
parseInstruction s =
  case readMaybe $ T.unpack (T'.tail s) of
    Just steps -> (dir, steps)
    Nothing -> error ("Could not read " ++ T.unpack s)
  where
    dir = (parseDirection . fromString) [T'.head s]

parseInstructions :: Text -> [Instruction]
parseInstructions s = parseInstruction <$> T'.splitOn "," s

-- (|+|) :: Position -> Position -> Position
-- (x0, y0) |+| (x1, y1) = (x0 + x1, y0 + y1)

prodPos :: Int -> Position -> Position
prodPos r (x, y) = (r * x, r * y)

neighbours :: Position -> [Position]
neighbours (x, y) =
  [ (x + 1, y + 1),
    (x + 1, y - 1),
    (x + 1, y),
    (x - 1, y + 1),
    (x - 1, y - 1),
    (x - 1, y),
    (x, y + 1),
    (x, y - 1)
  ]

circle :: Position -> Int -> [Position]
circle p r = prodPos r <$> neighbours p

scan :: Position -> [Position]
scan p = concat [prodPos r <$> neighbours p | r <- [1 .. 10000]]

getDistToClosestIntersection :: Int -> [Instruction] -> [Instruction] -> Int
getDistToClosestIntersection r is0 is1 =
  let matches = \p -> (p `Set.member` (routeStepsSet0 `Set.intersection` routeStepsSet1)) && (p /= centre)
   in case L.find matches (circle centre r) of
        Just p -> distance centre p
        Nothing ->
          if r < 10000
            then getDistToClosestIntersection (r + 1) is0 is1
            else error ("Could not find intersection in a radius of " ++ show r)
  where
    routeStepsSet0 = Set.fromList $ getRouteFromCenter is0
    routeStepsSet1 = Set.fromList $ getRouteFromCenter is1

loadData :: FilePath -> IO ([Instruction], [Instruction])
loadData f = do
  fileContents <- readFileUtf8 f
  -- pPrint fileContents
  let (isStr0, isStr1) = T'.breakOn "\n" fileContents
  pPrint isStr0
  pPrint (T'.tail isStr1)
  pure (parseInstructions isStr0, parseInstructions (T'.tail isStr1))

calculateFirstResult :: FilePath -> IO Text
calculateFirstResult p = do
  (is0, is1) <- loadData p
  let res = getDistToClosestIntersection 0 is0 is1
  pure $ (T.pack . show) res
