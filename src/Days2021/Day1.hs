{--# LANGUAGE NoImplicitPrelude #-}

module Days2021.Day1 where

import qualified Data.List.Split as S
import qualified Data.Maybe as M
import Import
import qualified RIO.List as L
import qualified RIO.List.Partial as L'
import qualified RIO.Text as T

-- data Window = Window
--   { windowIndex :: Int,
--     windowReadings :: (Int, Int, Int)
--   }
--   deriving (Eq, Ord, Show)

type Window = (Int, (Int, Int, Int))

mkWindow :: [(Int, Int)] -> Maybe Window
mkWindow [(i, a), (_, b), (_, c)] = Just (i, (a, b, c))
mkWindow [(i, a), (_, b)] = Just (i, (a, b, 0))
mkWindow [(i, a)] = Just (i, (a, 0, 0))
mkWindow _ = Nothing

getWindowSequence :: [(Int, Int)] -> [Window]
getWindowSequence [] = []
getWindowSequence xs =
  let nextWindowData = L'.init $ L.take 4 xs
   in case mkWindow nextWindowData of
        Just newWindow -> newWindow : getWindowSequence (L.drop 4 xs)
        Nothing -> getWindowSequence (L.drop 4 xs)

getWindowSequenceAt :: Int -> [Int] -> [Window]
getWindowSequenceAt i xs =
  getWindowSequence (drop i pairs)
  where
    pairs = L.zip ([0, 1 ..] :: [Int]) xs

getWindowAt :: [(Int, Int)] -> [Window]
getWindowAt (x@(i, _) : xs) = f <$> L.take 3 $ L.drop i (x : xs)
  where
    f ys = case mkWindow ys of
      Just w -> [w]
      Nothing -> []
getWindowAt [] = []

generateWindows :: [Int] -> [Window]
generateWindows xs =
  L.sort $ L.concatMap getWindowAt $ L.tails pairs
  where
    pairs = L.zip ([0, 1 ..] :: [Int]) xs

compareInitialPair :: [Int] -> Int
compareInitialPair [] = 0
compareInitialPair [_] = 0
compareInitialPair (x0 : x1 : _) =
  if x0 < x1
    then 1
    else 0

countIncreases :: [Int] -> Int
countIncreases xs =
  L.sum $ L.map compareInitialPair (L.tails xs)

loadReadings :: FilePath -> IO [Int]
loadReadings f = do
  fileContents <- readFileUtf8 f
  let ns = read <$> lines (T.unpack fileContents)
  pure ns

calculateResult :: ([Int] -> Int) -> FilePath -> IO Text
calculateResult f p = do
  xs <- loadReadings p
  pure $ (T.pack . show) $ f xs

calculateFirstResult :: FilePath -> IO Text
calculateFirstResult = calculateResult countIncreases