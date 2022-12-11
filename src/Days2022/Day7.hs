{-# LANGUAGE OverloadedStrings #-}

module Days2022.Day7 where

import Import
import qualified RIO.List as L
import qualified RIO.List.Partial as L'
import RIO.Partial (fromJust)
import RIO.Set (Set (..))
import qualified RIO.Set as S
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
  deriving (Eq, Ord, Show)

newtype Dir = Dir Text
  deriving (Eq, Ord, Show)

data FileTree = Leaf File | Node Dir (Set FileTree)
  deriving (Eq, Ord, Show)

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
        then Node (Dir dirName) (insertIfNotPresent children)
        else Node (Dir dirName) $ S.map (addFileTree (Dir targetDirName) newFt) children
  where
    isChild (Node (Dir newDirName) _) (Node (Dir existingDirName) _) = newDirName == existingDirName
    isChild _ _ = False
    insertIfNotPresent children =
      if S.size (S.filter (isChild newFt) children) == 0
        then S.insert newFt children
        else children

getParentDir :: Dir -> FileTree -> Maybe Dir
getParentDir (Dir childDirName) (Node dir children) =
  let isNodeWithSameName (Node (Dir dirName) _) = childDirName == dirName
      isNodeWithSameName _ = False
      childrenWithSameName = S.filter isNodeWithSameName children
      getThisDirParent = getParentDir (Dir childDirName)
      childrenList = S.toList children
   in if S.size childrenWithSameName == 1
        then Just dir
        else listToMaybe $ L.concatMap (maybeToList . getThisDirParent) childrenList
getParentDir _ _ = Nothing

getSubtree :: Dir -> FileTree -> Maybe FileTree
getSubtree (Dir targetDirName) subtree@(Node (Dir dirName) children) =
  if targetDirName == dirName
    then Just subtree
    else listToMaybe $ L.concatMap (maybeToList . getSubtree (Dir targetDirName)) $ S.toList children
getSubtree _ (Leaf _) = Nothing

getFileSizeSum :: FileTree -> Int
getFileSizeSum (Leaf (File n _)) = n
getFileSizeSum (Node _ children) = L.sum $ S.toList $ S.map getFileSizeSum children

parseFileTree :: [ParsedLine] -> Dir -> FileTree -> FileTree
parseFileTree [] _ currentFt = currentFt
parseFileTree (parsedLine : pls) wd currentFt = case parsedLine of
  ParsedCommand (Cd Root) -> parseFileTree pls (Dir "/") currentFt
  ParsedCommand (Cd Parent) -> parseFileTree pls (fromJust $ getParentDir wd currentFt) currentFt
  ParsedCommand (Cd (Child dirName)) -> parseFileTree pls (Dir dirName) currentFt
  ParsedCommand Ls -> parseFileTree pls wd currentFt
  ParsedDir newDir -> parseFileTree pls wd $ addFileTree wd (Node newDir S.empty) currentFt
  ParsedFile newFile -> parseFileTree pls wd $ addFileTree wd (Leaf newFile) currentFt
