
-- forecast.io API

module Forecast where

import Data.ByteString.Lazy
import Network.Wreq



type AccessToken = String

data Location =
  Location {
    latitude :: Float,
    longitude :: Float
  }
  deriving (Show, Eq)



apiHost = "https://api.forecast.io/forecast/"

getWeather :: AccessToken -> Location -> IO (Response ByteString)
getWeather accessToken (Location lat lon) = get uri
  where
  uri = apiHost ++ accessToken ++ "/" ++ (show lat) ++ "," ++ (show lon)



-- Location Aliases

montreal = Location 45.501689 (-73.567256)
