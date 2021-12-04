{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Days2021.Day2 where

import Import
import qualified RIO.Text as T

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

bearingMove :: Bearing -> [Command] -> Bearing
bearingMove p [] = p
bearingMove (a, (x, y)) (F n : cs) = bearingMove (a, (x + n, y + (a * n))) cs
bearingMove (a, (x, y)) (U n : cs) = bearingMove (a - n, (x, y)) cs
bearingMove (a, (x, y)) (D n : cs) = bearingMove (a + n, (x, y)) cs

loadCommands :: FilePath -> IO [Command]
loadCommands f = do
  fileContents <- readFileUtf8 f
  let cs = parseCommand <$> T.lines fileContents
  pure cs

calculateResult :: ([Command] -> Position) -> FilePath -> IO Text
calculateResult f p = do
  cs <- loadCommands p
  let (x, y) = f cs
  pure $ (T.pack . show) (x * y)

calculateFirstResult :: FilePath -> IO Text
calculateFirstResult = calculateResult (stepMove (0, 0))