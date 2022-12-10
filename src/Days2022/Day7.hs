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

-- data Node = Dir !Text | File !Int !Text
--   deriving (Eq, Show)

data File = File !Int !Text
  deriving (Eq, Show)

newtype Dir = Dir Text
  deriving (Eq, Show)

data FileTree = Leaf File | Node Dir [FileTree]
  deriving (Eq, Show)

data ParsedLine = ParsedCommand !Command | ParsedDir !Dir | ParsedFile !File
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

parseLine :: Text -> ParsedLine
parseLine l
  | (T.unpack l =~ ("^\\$ .*$" :: String)) :: Bool = ParsedCommand $ parseCommand l
  | (T.unpack l =~ ("^dir .*$" :: String)) :: Bool = ParsedDir $ Dir $ T.drop 4 l
  | (T.unpack l =~ ("^[0-9]+ .*$" :: String)) :: Bool = ParsedFile $ uncurry File (parseFile l)
  | otherwise = undefined

addFileTree :: Dir -> FileTree -> FileTree -> FileTree
addFileTree (Dir targetDirName) newFt ft =
  case ft of
    Leaf _ -> ft
    Node (Dir dirName) children ->
      if dirName == targetDirName
        then Node (Dir dirName) (children ++ [newFt])
        else Node (Dir dirName) $ addFileTree (Dir targetDirName) newFt <$> children

parseFileTree :: [Text] -> FileTree
parseFileTree = undefined
