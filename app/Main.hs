{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Import
import Options.Applicative.Simple
import qualified Paths_advent_of_code
import RIO.Process
import Run

main :: IO ()
main = do
  (options, ()) <-
    simpleOptions
      $(simpleVersion Paths_advent_of_code.version)
      "Advent of Code"
      "Solutions for Advent of Code 2019"
      ( Options
          <$> switch
            ( long "verbose"
                <> short 'v'
                <> help "Verbose output?"
            )
          <*> argument
            str
            ( metavar "exercise"
            )
      )
      empty
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app =
          App
            { appLogFunc = lf,
              appProcessContext = pc,
              appOptions = options
            }
     in runRIO app run
