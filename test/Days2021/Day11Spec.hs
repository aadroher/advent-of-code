{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Days2021.Day11Spec (spec) where

import Days2021.Day11
  ( Octopus (..),
    evolve,
    parseRow,
  )
import RIO
import Test.Hspec

spec :: Spec
spec = do
  describe "exercise 1" $ do
    describe "evolve" $ do
      let initialGrid =
            parseRow
              <$> [ "5483143223",
                    "2745854711",
                    "5264556173",
                    "6141336146",
                    "6357385478",
                    "4167524645",
                    "2176841721",
                    "6882881134",
                    "4846848554",
                    "5283751526"
                  ]
      it "returns the right grid for 3 steps" $ do
        let expectedGrid =
              parseRow
                <$> [ "0050900866",
                      "8500800575",
                      "9900000039",
                      "9700000041",
                      "9935080063",
                      "7712300000",
                      "7911250009",
                      "2211130000",
                      "0421125000",
                      "0021119000"
                    ]
        evolve initialGrid 3 `shouldBe` expectedGrid