{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wreq
import Control.Lens
import Data.Aeson.Lens
import Data.Maybe



apiHost = "https://api.forecast.io/forecast/"
apiKey = "db4db26b0bb9053853a23e8565ce7be9/"

main :: IO ()
main = do
  r <- get (apiHost ++ apiKey ++ "45.501689,-73.567256")
  let summary = fromJust $ r ^? responseBody . key "hourly" . key "summary"
  print summary
