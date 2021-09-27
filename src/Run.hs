{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run (run) where

import qualified Days.Day1 as D1
import Import
import qualified RIO.Text as T

getResult :: String -> RIO App Text
getResult "1.1" = do
  res <- liftIO $ D1.calculateResult "./data/day1-1.txt"
  pure $ T.pack $ show res
getResult _ = undefined

run :: RIO App ()
run = do
  logInfo "Hohoho, the Advent of Code!"
  app <- ask
  let e = exerciseName $ appOptions app
  logInfo $ displayShow $ "Results for day " ++ e ++ ":"
  res1 <- getResult e
  logInfo $ displayShow res1