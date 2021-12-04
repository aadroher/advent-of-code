{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Days2021.Day2 where

import Import
import qualified RIO.Text as T
import Util (calculateResult)

type Position = (Int, Int)

type Bearing = (Int, Position)

data Command = F Int | U Int | D Int
  deriving (Eq, Show)

parseCommand :: Text -> Command
parseCommand (T.stripPrefix "forward " -> Just ds) = F (read $ T.unpack ds)
parseCommand (T.stripPrefix "up " -> Just ds) = U (read $ T.unpack ds)
parseCommand (T.stripPrefix "down " -> Just ds) = D (read $ T.unpack ds)
parseCommand _ = undefined

stepMove :: Position -> [Command] -> Position
stepMove p [] = p
stepMove (x, y) (F n : cs) = stepMove (x + n, y) cs
stepMove (x, y) (U n : cs) = stepMove (x, y - n) cs
stepMove (x, y) (D n : cs) = stepMove (x, y + n) cs

stepsResult :: [Command] -> Int
stepsResult cs = x * y
  where
    (x, y) = stepMove (0, 0) cs

bearingMove :: Bearing -> [Command] -> Bearing
bearingMove p [] = p
bearingMove (a, (x, y)) (F n : cs) = bearingMove (a, (x + n, y + (a * n))) cs
bearingMove (a, (x, y)) (U n : cs) = bearingMove (a - n, (x, y)) cs
bearingMove (a, (x, y)) (D n : cs) = bearingMove (a + n, (x, y)) cs

bearingResult :: [Command] -> Int
bearingResult cs = x * y
  where
    (_, (x, y)) = bearingMove (0, (0, 0)) cs

calculateFirstResult :: FilePath -> IO Text
calculateFirstResult = calculateResult parseCommand stepsResult

calculateSecondResult :: FilePath -> IO Text
calculateSecondResult = calculateResult parseCommand bearingResult