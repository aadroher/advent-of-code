{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Days2021.Day2 where

import Import
import qualified RIO.Text as T

type Position = (Int, Int)

data Command = F Int | U Int | D Int
  deriving (Eq, Show)

parseCommand :: Text -> Command
parseCommand (T.stripPrefix "forward " -> Just ds) = F (read $ T.unpack ds)
parseCommand (T.stripPrefix "up " -> Just ds) = U (read $ T.unpack ds)
parseCommand (T.stripPrefix "down " -> Just ds) = D (read $ T.unpack ds)
parseCommand _ = undefined

move :: Position -> [Command] -> Position
move p [] = p
move (x, y) (F n : cs) = move (x + n, y) cs
move (x, y) (U n : cs) = move (x, y - n) cs
move (x, y) (D n : cs) = move (x, y + n) cs

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
calculateFirstResult = calculateResult (move (0, 0))