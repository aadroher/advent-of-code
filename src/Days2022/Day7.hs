{-# LANGUAGE OverloadedStrings #-}

module Days2022.Day7
  ( Command (..),
    Dir (..),
    DirReference (..),
    File (..),
    FileTree (..),
    ParsedLine (..),
    addFileTree,
    getDirs,
    getFileSizeSum,
    getFileTreesOfSizeLE,
    getParentDir,
    getSizeSumOfFileTreesOfSizeLE,
    getSubtree,
    parseFileTree,
    parseFileTreeFromRoot,
    parseLine,
    calculateFirstResult,
  )
where

import Import
import qualified RIO.List as L
import qualified RIO.List.Partial as L'
import RIO.Partial (fromJust)
import RIO.Set (Set (..))
import qualified RIO.Set as S
import qualified RIO.Text as T
import Text.Pretty.Simple (pShow, pString)
import Text.Regex.TDFA ((=~))
import Util (calculateResult)

data DirReference = RootReference | ParentReference | ChildReference !Text
  deriving (Eq, Show)

data Command = Cd !DirReference | Ls
  deriving (Eq, Show)

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
  "$ cd /" -> RootReference
  "$ cd .." -> ParentReference
  _ -> ChildReference $ T.drop 5 l

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
getParentDir (Dir "/") _ = Nothing
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
    else
      listToMaybe $
        L.concatMap (maybeToList . getSubtree (Dir targetDirName)) $
          S.toList children
getSubtree _ (Leaf _) = Nothing

getDirs :: FileTree -> [Dir]
getDirs (Leaf _) = []
getDirs (Node dir children) = dir : L.concatMap getDirs (S.toList children)

getFileSizeSum :: FileTree -> Int
getFileSizeSum (Leaf (File n _)) = n
getFileSizeSum (Node _ children) = L.sum $ S.toList $ S.map getFileSizeSum children

getFileTreesOfSizeLE :: Int -> FileTree -> [(Dir, Int)]
getFileTreesOfSizeLE n ft =
  let fileTreeSizes = (\dir -> (dir, (getFileSizeSum . fromJust) (getSubtree dir ft))) <$> getDirs ft
   in L.filter ((n >=) . snd) fileTreeSizes

getSizeSumOfFileTreesOfSizeLE :: Int -> FileTree -> Int
getSizeSumOfFileTreesOfSizeLE n ft = L.sum $ snd <$> getFileTreesOfSizeLE n ft

parseFileTree :: [ParsedLine] -> Dir -> FileTree -> FileTree
parseFileTree [] _ currentFt = currentFt
parseFileTree (parsedLine : pls) currentDir currentFt = case parsedLine of
  ParsedCommand (Cd RootReference) -> parseFileTree pls (Dir "/") currentFt
  ParsedCommand (Cd ParentReference) -> parseFileTree pls (fromJust $ getParentDir currentDir currentFt) currentFt
  ParsedCommand (Cd (ChildReference dirName)) -> parseFileTree pls (Dir dirName) currentFt
  ParsedCommand Ls -> parseFileTree pls currentDir currentFt
  ParsedDir newDir -> parseFileTree pls currentDir $ addFileTree currentDir (Node newDir S.empty) currentFt
  ParsedFile newFile -> parseFileTree pls currentDir $ addFileTree currentDir (Leaf newFile) currentFt

parseFileTreeFromRoot :: [ParsedLine] -> FileTree
parseFileTreeFromRoot pls = parseFileTree pls (Dir "/") (Node (Dir "/") S.empty)

calculateFirstResult :: FilePath -> IO Text
calculateFirstResult =
  calculateResult
    parseLine
    (getSizeSumOfFileTreesOfSizeLE 100000 . parseFileTreeFromRoot)