{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Days2022.Day1Spec (spec) where

import Days2022.Day1
  ( getMaxCalorieCount,
  )
import Import
import Test.Hspec

spec :: Spec
spec = do
  describe "exercise 1" $ do
    describe "getMaxCalorieCount" $ do
      it "solves the example" $ do
        let calorieBatches =
              [ [ 1000,
                  2000,
                  3000
                ],
                [ 4000
                ],
                [ 5000,
                  6000
                ],
                [ 7000,
                  8000,
                  9000
                ],
                [ 10000
                ]
              ]

        getMaxCalorieCount calorieBatches `shouldBe` 24000