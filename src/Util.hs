{-# LANGUAGE NoImplicitPrelude #-}

-- | Silly utility module, used to demonstrate how to write a test
-- case.
module Util where

import RIO
import qualified RIO.Text as T

calculateResult :: Show b => (Text -> a) -> ([a] -> b) -> FilePath -> IO Text
calculateResult parse process filePath = do
  fileContents <- readFileUtf8 filePath
  let inputValues = parse <$> T.lines fileContents
  pure $ (T.pack . show) $ process inputValues