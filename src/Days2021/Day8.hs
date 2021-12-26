{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Days2021.Day8 where

import Import
import qualified RIO.List as L
import qualified RIO.List.Partial as L'
import qualified RIO.Map as M
import RIO.Map.Partial ((!))
import RIO.Set ((\\))
import qualified RIO.Set as S
import qualified RIO.Text as T
import Util (calculateResult)

data Wire = WA | WB | WC | WD | WE | WF | WG
  deriving (Eq, Ord, Show)

type Signal = Set Wire

type Entry = (Set Signal, [Signal])

data Segment = Top | Mid | Bot | LTop | RTop | LBot | RBot
  deriving (Eq, Ord, Show)

type Connection = (Wire, Segment)

segmentsToInt :: Map (Set Segment) Int
segmentsToInt =
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

allConnections :: Set Connection
allConnections =
  S.fromList
    [ (s, p)
      | s <- [WA, WB, WC, WD, WE, WF, WG],
        p <- [Top, Mid, Bot, LTop, RTop, LBot, RBot]
    ]

parseWire :: Text -> Wire
parseWire t = case t of
  "a" -> WA
  "b" -> WB
  "c" -> WC
  "d" -> WD
  "e" -> WE
  "f" -> WF
  "g" -> WG
  _ -> error $ "Could not parse segment: " ++ T.unpack t

parseSignal :: Text -> Signal
parseSignal t = S.fromList $ parseWire . (`T.cons` "") <$> T.unpack t

parseEntry :: Text -> Entry
parseEntry t =
  ( S.fromList $ parseSignal <$> signalTexts,
    parseSignal <$> digitTexts
  )
  where
    [signalsText, digitsText] = T.split (== '|') t
    signalTexts = T.words signalsText
    digitTexts = T.words digitsText

numSegments :: Int -> Int
numSegments n =
  S.size $ L'.head $ M.keys singleton
  where
    singleton = M.filter (== n) segmentsToInt

signalsForIntWithSameNumSegments :: Int -> Set Signal -> Set Signal
signalsForIntWithSameNumSegments n = S.filter ((== numSegments n) . S.size)

getTopWire :: Set Signal -> Wire
getTopWire ss =
  L'.head $ S.toList $ sevenSignal \\ oneSignal
  where
    oneSignal = L'.head $ S.toList $ signalsForIntWithSameNumSegments 1 ss
    sevenSignal = L'.head $ S.toList $ signalsForIntWithSameNumSegments 7 ss

getMidWire :: Set Signal -> Wire
getMidWire ss =
  L'.head $
    S.toList $
      S.filter
        ( \w ->
            S.size (S.filter (S.member w) sixSegmentSignals) /= S.size sixSegmentSignals
        )
        ltopAndMidWires
  where
    oneSignal = L'.head $ S.toList $ signalsForIntWithSameNumSegments 1 ss
    fourSignal = L'.head $ S.toList $ signalsForIntWithSameNumSegments 4 ss
    sixSegmentSignals = signalsForIntWithSameNumSegments 0 ss
    ltopAndMidWires = fourSignal \\ oneSignal

getBotWire :: Set Signal -> Wire
getBotWire ss =
  L'.head $
    S.toList $
      threeSignal \\ S.union oneSignal (S.fromList [getTopWire ss, getMidWire ss])
  where
    oneSignal = L'.head $ S.toList $ signalsForIntWithSameNumSegments 1 ss
    fiveSegmentSignals = signalsForIntWithSameNumSegments 2 ss
    threeSignal =
      L'.head $
        S.toList $
          S.filter
            (S.isSubsetOf oneSignal)
            fiveSegmentSignals

getLTopWire :: Set Signal -> Wire
getLTopWire ss =
  L'.head $
    S.toList $
      fourSignal \\ S.union oneSignal (S.singleton $ getMidWire ss)
  where
    oneSignal = L'.head $ S.toList $ signalsForIntWithSameNumSegments 1 ss
    fourSignal = L'.head $ S.toList $ signalsForIntWithSameNumSegments 4 ss

getLBotWire :: Set Signal -> Wire
getLBotWire ss =
  L'.head $
    S.toList $
      eightSignal \\ S.union fourSignal (S.fromList [getTopWire ss, getBotWire ss])
  where
    fourSignal = L'.head $ S.toList $ signalsForIntWithSameNumSegments 4 ss
    eightSignal = L'.head $ S.toList $ signalsForIntWithSameNumSegments 8 ss

getRTopWire :: Set Signal -> Wire
getRTopWire ss =
  L'.head $ S.toList $ oneSignal \\ sixSignal
  where
    oneSignal = L'.head $ S.toList $ signalsForIntWithSameNumSegments 1 ss
    sixSegmentSignals = signalsForIntWithSameNumSegments 0 ss
    sixSignal =
      L'.head $
        S.toList $
          S.filter
            (not . S.isSubsetOf oneSignal)
            sixSegmentSignals

getRBotWire :: Set Signal -> Wire
getRBotWire ss =
  L'.head $ S.toList $ oneSignal \\ S.singleton (getRTopWire ss)
  where
    oneSignal = L'.head $ S.toList $ signalsForIntWithSameNumSegments 1 ss

getConnections :: Set Signal -> Map Wire Segment
getConnections ss =
  M.fromList
    [ (getTopWire ss, Top),
      (getMidWire ss, Mid),
      (getBotWire ss, Bot),
      (getLTopWire ss, LTop),
      (getLBotWire ss, LBot),
      (getRTopWire ss, RTop),
      (getRBotWire ss, RBot)
    ]

getIntValue :: Set Signal -> Signal -> Int
getIntValue signals output =
  segmentsToInt ! segments
  where
    connections = getConnections signals
    segments = S.map (connections !) output

countTotalDigits :: [Entry] -> Int
countTotalDigits entries =
  L.length $ L.filter isEasyDigit digitsSignals
  where
    digitsSignals = L.concatMap snd entries
    isEasyDigit = \ds -> S.member (S.size ds) $ S.fromList [2, 4, 3, 7]

calculateOutputValue :: Entry -> Int
calculateOutputValue (signals, outputs) =
  L.sum $ (\(d, i) -> d * 10 ^ i) <$> L.zip (L.reverse digits) ([0 ..] :: [Int])
  where
    digits = getIntValue signals <$> outputs

addOutputValues :: [Entry] -> Int
addOutputValues entries = L.sum $ calculateOutputValue <$> entries

calculateFirstResult :: FilePath -> IO Text
calculateFirstResult = calculateResult parseEntry countTotalDigits

calculateSecondResult :: FilePath -> IO Text
calculateSecondResult = calculateResult parseEntry addOutputValues
