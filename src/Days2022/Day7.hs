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

data DirReference = Root | Parent | Child !Text
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

addFileTree :: [Dir] -> FileTree -> FileTree -> FileTree
addFileTree [Dir targetDirName] newFt node@(Node (Dir dirName) children) =
  if targetDirName == dirName
    then Node (Dir dirName) (S.insert newFt (S.filter (notChildWithRootName newFt) children))
    else node
  where
    notChildWithRootName (Node (Dir newSubRootName) _) (Node (Dir name) _) = name /= newSubRootName
    notChildWithRootName _ _ = True
addFileTree (_ : descendents) newFt (Node (Dir dirName) children) =
  Node (Dir dirName) $ S.map (addFileTreeIfNode descendents newFt) children
  where
    addFileTreeIfNode ds nft node@(Node _ _) = addFileTree ds nft node
    addFileTreeIfNode _ _ leaf@(Leaf _) = leaf
addFileTree _ _ (Leaf _) = error "cannot add tree to file"
addFileTree _ _ _ = error "addFileTree failed"

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

parseFileTree :: [ParsedLine] -> [Dir] -> FileTree -> FileTree
parseFileTree [] _ currentFt = currentFt
parseFileTree (parsedLine : pls) currentPath currentFt = case parsedLine of
  ParsedCommand (Cd Root) -> parseFileTree pls [Dir "/"] currentFt
  ParsedCommand (Cd Parent) -> case currentPath of
    (_ : (parentDir : ancestors)) -> parseFileTree pls (parentDir : ancestors) currentFt
    _ -> undefined
  ParsedCommand (Cd (Child dirName)) -> parseFileTree pls (Dir dirName : currentPath) currentFt
  ParsedCommand Ls -> parseFileTree pls currentPath currentFt
  ParsedDir newDir -> parseFileTree pls currentPath $ addFileTree (L.reverse currentPath) (Node newDir S.empty) currentFt
  ParsedFile newFile -> parseFileTree pls currentPath $ addFileTree (L.reverse currentPath) (Leaf newFile) currentFt

parseFileTreeFromRoot :: [ParsedLine] -> FileTree
parseFileTreeFromRoot pls = parseFileTree pls [Dir "/"] (Node (Dir "/") S.empty)

calculateFirstResult :: FilePath -> IO Text
calculateFirstResult =
  calculateResult
    parseLine
    (getSizeSumOfFileTreesOfSizeLE 100000 . parseFileTreeFromRoot)