{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Days2021.Day8 where

import Import
import qualified RIO.List as L
import RIO.Map (Map)
import qualified RIO.Map as M
import RIO.Set (Set, (\\))
import qualified RIO.Set as S
import qualified RIO.Text as T
import Util (calculateResult)

data Segment = SA | SB | SC | SD | SE | SF | SG
  deriving (Eq, Ord, Show)

type Signal = Set Segment

type Entry = ([Signal], [Signal])

data DisplayPosition = Top | Mid | Bot | LTop | RTop | LBot | RBot
  deriving (Eq, Ord, Show)

type Decoding = (Signal, Maybe Int)

type SegmentMapping = (Segment, DisplayPosition)

type Constraints = Set SegmentMapping

displayIntMapping :: Map (Set DisplayPosition) Int
displayIntMapping =
  M.fromList $
    first S.fromList
      <$> [ ([Top, Bot, LTop, RTop, LBot, RBot], 0),
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

universalContraints :: Set SegmentMapping
universalContraints =
  S.fromList
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
parseSignal t = S.fromList $ parseSegment . (`T.cons` "") <$> T.unpack t

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
isDigit 1 = (== 2) . S.size
isDigit 4 = (== 4) . S.size
isDigit 7 = (== 3) . S.size
isDigit 8 = (== 7) . S.size
isDigit _ = error "Cannot identify digit"

reduceConstraints :: Set SegmentMapping -> Set SegmentMapping -> Set SegmentMapping
reduceConstraints c sms =
  c
    \\ S.fromList
      [ (s, p)
        | s <- S.toList segmentsToRemove,
          p <- S.toList receivedImages
      ]
  where
    receivedRange = S.map fst sms
    segmentsToRemove = S.fromList [SA, SB, SC, SD, SE, SF, SG] \\ receivedRange
    receivedImages = S.map snd sms

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