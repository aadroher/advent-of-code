{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Days2021.Day8Spec (spec) where

import Days2021.Day8
  ( Wire (WA, WB, WC, WD, WE, WF, WG),
    addOutputValues,
    calculateOutputValue,
    countTotalDigits,
    getBotWire,
    getIntValue,
    getLBotWire,
    getLTopWire,
    getMidWire,
    getRBotWire,
    getRTopWire,
    getTopWire,
    parseEntry,
    parseSignal,
  )
import Import
import qualified RIO.List.Partial as L'
import qualified RIO.Set as S
import Test.Hspec

spec :: Spec
spec = do
  describe "AoC 2021 Day 8" $ do
    describe "parseSignal" $ do
      it "parses 'abc'" $ do
        parseSignal "abc" `shouldBe` S.fromList [WA, WB, WC]
    describe "parseEntry" $ do
      it "parses first example" $ do
        parseEntry "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
          `shouldBe` ( S.fromList
                         [ S.fromList [WA, WB, WC, WD, WE, WF, WG],
                           S.fromList [WB, WC, WD, WE, WF],
                           S.fromList [WA, WC, WD, WF, WG],
                           S.fromList [WA, WB, WC, WD, WF],
                           S.fromList [WA, WB, WD],
                           S.fromList [WA, WB, WC, WD, WE, WF],
                           S.fromList [WB, WC, WD, WE, WF, WG],
                           S.fromList [WA, WB, WE, WF],
                           S.fromList [WA, WB, WC, WD, WE, WG],
                           S.fromList [WA, WB]
                         ],
                       [ S.fromList [WB, WC, WD, WE, WF],
                         S.fromList [WA, WB, WC, WD, WF],
                         S.fromList [WB, WC, WD, WE, WF],
                         S.fromList [WA, WB, WC, WD, WF]
                       ]
                     )
    describe "getTopWire" $ do
      it "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab -> d" $ do
        let (signals, _) = parseEntry "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
        getTopWire signals `shouldBe` WD
    describe "getMidWire" $ do
      it "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab -> f" $ do
        let (signals, _) = parseEntry "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
        getMidWire signals `shouldBe` WF
    describe "getBotWire" $ do
      it "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab -> c" $ do
        let (signals, _) = parseEntry "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
        getBotWire signals `shouldBe` WC
    describe "getLTopWire" $ do
      it "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab -> e" $ do
        let (signals, _) = parseEntry "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
        getLTopWire signals `shouldBe` WE
    describe "getLBotWire" $ do
      it "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab -> g" $ do
        let (signals, _) = parseEntry "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
        getLBotWire signals `shouldBe` WG
    describe "getRTopWire" $ do
      it "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab -> a" $ do
        let (signals, _) = parseEntry "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
        getRTopWire signals `shouldBe` WA
    describe "getRBotWire" $ do
      it "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab -> b" $ do
        let (signals, _) = parseEntry "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
        getRBotWire signals `shouldBe` WB
    describe "countTotalDigits" $ do
      it "solves example" $ do
        let entries =
              parseEntry
                <$> [ "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe",
                      "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc",
                      "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg",
                      "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb",
                      "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea",
                      "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb",
                      "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe",
                      "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef",
                      "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb",
                      "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"
                    ]
        countTotalDigits entries `shouldBe` 26
    describe "getIntValue" $ do
      it "cdfeb -> 5" $ do
        let (signals, outputs) = parseEntry "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
        getIntValue signals (L'.head outputs) `shouldBe` 5
    describe "calculateOutputValue" $ do
      it "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf" $ do
        let entry = parseEntry "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
        calculateOutputValue entry `shouldBe` 5353
    describe "addOutputValues" $ do
      it "solves example" $ do
        let entries =
              parseEntry
                <$> [ "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe",
                      "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc",
                      "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg",
                      "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb",
                      "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea",
                      "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb",
                      "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe",
                      "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef",
                      "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb",
                      "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"
                    ]
        addOutputValues entries `shouldBe` 61229