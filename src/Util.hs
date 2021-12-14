{-# LANGUAGE NoImplicitPrelude #-}

-- | Silly utility module, used to demonstrate how to write a test
-- case.
module Util where

import RIO
import qualified RIO.List as L
import qualified RIO.Text as T

calculateResult :: Show b => (Text -> a) -> ([a] -> b) -> FilePath -> IO Text
calculateResult parse process filePath = do
  fileContents <- readFileUtf8 filePath
  let inputValues = parse <$> T.lines fileContents
  pure $ (T.pack . show) $ process inputValues

getFilePath :: String -> FilePath
getFilePath exName = "./data/" ++ year ++ "/day" ++ num ++ ".txt"
  where
    year = L.take 2 exName
    num = (L.drop 3 . L.take 4) exName

getResult :: (FilePath -> IO Text) -> String -> IO Text
getResult solve exName = do
  res <- liftIO $ solve $ getFilePath exName
  pure $ T.pack $ show res