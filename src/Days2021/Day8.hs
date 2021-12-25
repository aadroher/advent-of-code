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

data Wire = WA | WB | WC | WD | WE | WF | WG
  deriving (Eq, Ord, Show)

type Signal = Set Wire

type Entry = (Set Signal, [Signal])

data Segment = Top | Mid | Bot | LTop | RTop | LBot | RBot
  deriving (Eq, Ord, Show)

type Connection = (Wire, Segment)

choose :: [b] -> Int -> [[b]]
_ `choose` 0 = [[]]
[] `choose` _ = []
(x : xs) `choose` k = (x :) <$> (xs `choose` (k -1)) ++ xs `choose` k

subsetsOfSize :: Ord a => Int -> Set a -> Set (Set a)
subsetsOfSize k s =
  S.fromList (S.fromList <$> members `choose` k)
  where
    members = S.toList s

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

easyDigits :: [Int]
easyDigits = [1, 4, 7, 8]

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

candidateSegmentSets :: Signal -> Set (Set Segment)
candidateSegmentSets wires =
  S.filter
    (\dps -> S.size dps == S.size wires)
    segmentSets
  where
    segmentSets = S.fromList $ M.keys segmentsToInt

connectionsFor :: Signal -> Set Connection -> Set Connection
connectionsFor wires = S.filter $ \(s, _) -> S.member s wires

isValidMappingFor :: Signal -> Set Connection -> Bool
isValidMappingFor wires connections = S.size (connectionsFor wires connections) == S.size wires

getDigitToPrint :: Signal -> Set Connection -> Maybe Int
getDigitToPrint wires connections =
  M.lookup (S.map snd $ connectionsFor wires connections) segmentsToInt

isResolvingConstraintSet :: Set Connection -> Signal -> Bool
isResolvingConstraintSet c = L.all hasUniqueImage
  where
    withDomain segment = S.filter $ \(s, _) -> s == segment
    hasUniqueImage segment =
      L.length (withDomain segment c) == 1

reduceConnections :: Set Connection -> Set Connection -> Set Connection
reduceConnections connections validConnections =
  connections
    \\ S.fromList
      [ (s, p)
        | s <- S.toList segmentsToRemove,
          p <- S.toList receivedImages
      ]
  where
    receivedRange = S.map fst validConnections
    segmentsToRemove = S.fromList [WA, WB, WC, WD, WE, WF, WG] \\ receivedRange
    receivedImages = S.map snd validConnections

-- countTotalDigits :: [Int] -> [Entry] -> Int
-- countTotalDigits digits es =
--   L.length $ L.filter isIdentifiableDigit digitsSignals
--   where
--     digitsSignals = L.concatMap snd es
--     isIdentifiableDigit = \s -> L.or $ (`isDigit` s) <$> digits

calculateOutputValue :: Entry -> Int
calculateOutputValue (signals, outputs) =
  undefined
  where
    reducedConnections = S.foldl reduceConnections S.empty allConnections
    candidateConnectionSets = subsetsOfSize 7 reduceConnections

calculateFirstResult :: FilePath -> IO Text
calculateFirstResult = undefined

-- calculateFirstResult = calculateResult parseEntry $ countTotalDigits easyDigit