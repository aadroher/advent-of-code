{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Days2021.Day8 where

import Data.Hashable
import Import
import qualified RIO.HashSet as HS
import qualified RIO.List as L
import qualified RIO.Text as T
import Util (calculateResult)

data Segment = SA | SB | SC | SD | SE | SF | SG
  deriving (Eq, Show)

instance Hashable Segment where
  hashWithSalt salt s =
    salt
      + case s of
        SA -> 0
        SB -> 1
        SC -> 2
        SD -> 3
        SE -> 4
        SF -> 5
        SG -> 6

type Signal = HashSet Segment

type Entry = ([Signal], [Signal])

data DisplayPosition = Top | Mid | Bot | LTop | RTop | LBot | RBot
  deriving (Eq, Show)

type Decoding = (Signal, Maybe Int)

type SegmentMapping = (Segment, DisplayPosition)

type Constraints = [SegmentMapping]

displayIntMapping :: [([DisplayPosition], Int)]
displayIntMapping =
  [ ([Top, Bot, LTop, RTop, LBot, RBot], 0),
    ([RTop, RBot], 1),
    ([Top, RTop, Mid, LBot, Bot], 2),
    ([Top, RTop, Mid, RBot, Bot], 3),
    ([LTop, RTop, Mid, RBot], 4),
    ([Top, LTop, Mid, RBot, Bot], 5),
    ([Top, LTop, Mid, LBot, RBot, Bot], 6),
    ([Top, RTop, RBot], 7),
    ([Top, Mid, Bot, LTop, RTop, LBot, RBot], 8),
    ([Top, LTop, RTop, Mid, RBot, Bot], 9)
  ]

easyDigits :: [Int]
easyDigits = [1, 4, 7, 8]

universalContraints :: Constraints
universalContraints =
  [ (s, p)
    | s <- [SA, SB, SC, SD, SE, SF, SG],
      p <- [Top, Mid, Bot, LTop, RTop, LBot, RBot]
  ]

parseSegment :: Text -> Segment
parseSegment t = case t of
  "a" -> SA
  "b" -> SB
  "c" -> SC
  "d" -> SD
  "e" -> SE
  "f" -> SF
  "g" -> SG
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

reduceConstraints :: Constraints -> [SegmentMapping] -> Constraints
reduceConstraints c sms = undefined

-- attemptEasyDecode :: Signal -> Maybe SegmentMapping
-- attemptEasyDecode s
--   | isDigit 1 s = (s, Just 1)
--   | isDigit 4 s = (s, Just 4)
--   | isDigit 7 s = (s, Just 7)
--   | isDigit 8 s = (s, Just 8)
--   | otherwise = (s, Nothing)

countTotalDigits :: [Int] -> [Entry] -> Int
countTotalDigits digits es =
  L.length $ L.filter isIdentifiableDigit digitsSignals
  where
    digitsSignals = L.concatMap snd es
    isIdentifiableDigit = \s -> L.or $ (`isDigit` s) <$> digits

calculateDecoding :: [Decoding] -> Signal -> Decoding
calculateDecoding ds = undefined

calculateOutputValue :: Entry -> Int
calculateOutputValue _ = 0

calculateFirstResult :: FilePath -> IO Text
calculateFirstResult = undefined

-- calculateFirstResult = calculateResult parseEntry $ countTotalDigits easyDigit