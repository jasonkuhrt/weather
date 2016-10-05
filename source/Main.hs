{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wreq (Response)
import qualified Network.Wreq as Wreq
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import System.Directory (getCurrentDirectory)
import Control.Lens ((^.), (^?))
import Data.Aeson
import Data.Aeson.Lens (key)
import WeatherPoint



main :: IO ()
main = do
  response <- forecastWeek
  let body  = response ^. Wreq.responseBody
  print body

demo :: IO ()
demo = do
  body <-
    BSL.readFile
    =<< (++ "/assets/example.json")
    <$> getCurrentDirectory
  print (fmap fromJSON . (^? key "currently") $ body :: Maybe (Result CurrentWeather))






forecastWeek :: IO (Response ByteString)
forecastWeek = Wreq.get $ concat [
    apiRoot,
    "/forecast/",
    accessToken,
    "/37.8267,-122.4233?exclude=flags,alerts,minutely,hourly"
  ]

apiRoot :: String
apiRoot = "https://api.darksky.net"

accessToken :: String
accessToken = "db4db26b0bb9053853a23e8565ce7be9"
