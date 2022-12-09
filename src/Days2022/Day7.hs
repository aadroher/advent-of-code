{-# LANGUAGE OverloadedStrings #-}

module Days2022.Day7 where

import Import
import qualified RIO.List as L
import qualified RIO.List.Partial as L'
import qualified RIO.Text as T
import Text.Regex.TDFA ((=~))
import Util (calculateResult)

data DirReference = Root | Parent | Child !Text
  deriving (Eq, Show)

data Command = Cd !DirReference | Ls
  deriving (Eq, Show)

data Node = Dir !Text | File !Int !Text
  deriving (Eq, Show)

data ParsedLine = ParsedCommand !Command | ParsedNode !Node
  deriving (Eq, Show)

parseDirReference :: Text -> DirReference
parseDirReference l = case l of
  "$ cd /" -> Root
  "$ cd .." -> Parent
  _ -> Child $ T.pack (((T.unpack l =~ ("^\\$ cd .*$" :: String))) :: String)

parseCommand :: Text -> Command
parseCommand l
  | (T.unpack l =~ ("^\\$ cd .*$" :: String)) :: Bool = Cd (parseDirReference l)
  | otherwise = undefined

parseLine :: Text -> ParsedLine
parseLine l = case T.unpack l of
  ('$' : _) -> ParsedCommand (parseCommand l)
  _ -> undefined