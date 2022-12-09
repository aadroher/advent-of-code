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
  _ -> Child $ T.drop 5 l

parseCommand :: Text -> Command
parseCommand l
  | (T.unpack l =~ ("^\\$ cd .*$" :: String)) :: Bool = Cd (parseDirReference l)
  | l == "$ ls" = Ls
  | otherwise = undefined

parseFile :: Text -> (Int, Text)
parseFile t =
  let [t0, t1] = T.split (== ' ') t
   in (read $ T.unpack t0, t1)

parseNode :: Text -> Node
parseNode l
  | (T.unpack l =~ ("^dir .*$" :: String)) :: Bool = Dir $ T.drop 4 l
  | (T.unpack l =~ ("^[0-9]+ .*$" :: String)) :: Bool = File (fst $ parseFile l) (snd $ parseFile l)
  | otherwise = undefined

parseLine :: Text -> ParsedLine
parseLine l = case T.unpack l of
  ('$' : _) -> ParsedCommand $ parseCommand l
  _ -> ParsedNode $ parseNode l