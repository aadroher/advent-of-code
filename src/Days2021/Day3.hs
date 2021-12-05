{-# LANGUAGE OverloadedStrings #-}

module Days2021.Day3 where

import Data.Tuple.Select as S
import Import
import qualified RIO.List as L
import qualified RIO.Text as T
import Util (calculateResult)

data Bit = Zero | One
  deriving (Eq, Show)

type BinNum = (Bit, Bit, Bit, Bit, Bit)

parseBit :: Char -> Bit
parseBit '0' = Zero
parseBit '1' = One
parseBit _ = undefined

parseBinNum :: Text -> BinNum
parseBinNum t =
  case parseBit <$> T.unpack t of
    [b0, b1, b2, b3, b4] -> (b0, b1, b2, b3, b4)
    _ -> error "Could not parse num"

bitToFloat :: Floating a => Bit -> a
bitToFloat Zero = 0.0
bitToFloat One = 1.0

binNumToInt :: BinNum -> Int
binNumToInt (b0, b1, b2, b3, b4) =
  sum $
    (\(i, b) -> round (bitToFloat b) * 2 ^ i)
      <$> zip [0 ..] (L.reverse [b0, b1, b2, b3, b4])

getCommonalityByComparator :: Floating a => (a -> a -> Bool) -> [Bit] -> Bit
getCommonalityByComparator comp bs =
  let avg = sum (bitToFloat <$> bs) / fromIntegral (L.length bs)
   in bool Zero One (avg `comp` 0.5)

getLeastCommon :: [Bit] -> Bit
getLeastCommon = getCommonalityByComparator (<)

getMostCommon :: [Bit] -> Bit
getMostCommon = getCommonalityByComparator (>)

getColumnAt :: Int -> [BinNum] -> [Bit]
getColumnAt i bns = sel <$> bns
  where
    sel = case i of
      0 -> S.sel1
      1 -> S.sel2
      2 -> S.sel3
      3 -> S.sel4
      4 -> S.sel5
      _ -> error "Index out of bounds"

calculateRate :: ([Bit] -> Bit) -> [BinNum] -> BinNum
calculateRate agg bs =
  ( colAgg 0,
    colAgg 1,
    colAgg 2,
    colAgg 3,
    colAgg 4
  )
  where
    colAgg = \i -> agg $ getColumnAt i bs

getGammaRate :: [BinNum] -> BinNum
getGammaRate = calculateRate getMostCommon

getEpsilonRate :: [BinNum] -> BinNum
getEpsilonRate = calculateRate getLeastCommon

getRatesProduct :: [BinNum] -> Int
getRatesProduct bs = gamma * epsilon
  where
    gamma = binNumToInt $ getGammaRate bs
    epsilon = binNumToInt $ getEpsilonRate bs

calculateFirstResult :: FilePath -> IO Text
calculateFirstResult = calculateResult parseBinNum getRatesProduct
