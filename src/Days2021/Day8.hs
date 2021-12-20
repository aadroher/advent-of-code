{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Days2021.Day8 where

import Data.Hashable
import Import
import qualified RIO.HashSet as HS
import qualified RIO.List as L
import qualified RIO.Text as T
import Util (calculateResult)

data Segment = A | B | C | D | E | F | G
  deriving (Eq, Show)

instance Hashable Segment where
  hashWithSalt salt s =
    salt
      + case s of
        A -> 0
        B -> 1
        C -> 2
        D -> 3
        E -> 4
        F -> 5
        G -> 6

type Signal = HashSet Segment

type Entry = ([Signal], [Signal])

parseSegment :: Text -> Segment
parseSegment t = case t of
  "a" -> A
  "b" -> B
  "c" -> C
  "d" -> D
  "e" -> E
  "f" -> F
  "g" -> G
  _ -> error $ "Could not parse segment: " ++ T.unpack t

parseSignal :: Text -> Signal
parseSignal t = HS.fromList $ parseSegment . (`T.cons` "") <$> T.unpack t

parseEntry :: Text -> Entry
parseEntry t =
  ( parseSignal <$> signalTexts,
    parseSignal <$> digitTexts
  )
  where
    [signalsText, digitsText] = T.split (== '|') t
    signalTexts = T.words signalsText
    digitTexts = T.words digitsText

isDigit :: Int -> Signal -> Bool
isDigit 1 = (== 2) . HS.size
isDigit 4 = (== 4) . HS.size
isDigit 7 = (== 3) . HS.size
isDigit 8 = (== 7) . HS.size
isDigit _ = error "Cannot identify digit"

countTotalDigits :: [Int] -> [Entry] -> Int
countTotalDigits digits es =
  L.length $ L.filter isIdentifiableDigit digitsSignals
  where
    digitsSignals = L.concatMap snd es
    isIdentifiableDigit = \s -> L.or $ (`isDigit` s) <$> digits

calculateFirstResult :: FilePath -> IO Text
calculateFirstResult = calculateResult parseEntry $ countTotalDigits [1, 4, 7, 8]