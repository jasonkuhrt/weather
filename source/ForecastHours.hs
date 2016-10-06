{-# LANGUAGE OverloadedStrings #-}

{- README

This module contains functions for creating a textual visualization of hourly-forecast data. An example of what the visualization looks like:

                      12 Hour Weather Forecast
                      ━━━━━━━━━━━━━━━━━━━━━━━━
                             ↗ Montreal



────────────────────────────────┬PM─────────────────────────────────
Time         h   Now 9   10  11 │12  1   2   3   4   5   6   7   8
────────────────────────────────┼───────────────────────────────────
                 ☀☁  ☂   ☀   ☁  │~   ☂   ☂   ☀☁  ☀☁  ☀☁  ☀   ☀   ☀
                                │
Temperature °c   5   7   11  14 │15  15  16  14  13  13  11  7   6
                                │
                 ▄▄▄▄▅▅▅▅▆▆▆▆███│████████████████▇▇▇▇▇▇▇▇▆▆▆▆▅▅▅▅▅▅▅
                                │
Rain        cm   .05 .06 .04 0  │0   0   0   0   0   0   0   0   0
                                │
                 ▄▄▄▄▅▅▅▅▃▃▃▃▁▁▁│▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
                                │
Cloud Cover  %   100 100 100 100│99  87  76  85  90  95  99  90  100
                                │
                 ███████████████│████▇▇▇▇▄▄▄▄▇▇▇▇▇▇▇▇████████▇▇▇▇███
                                │
Humidity     %   60  61  66  32 │16  14  40  87  29  10  99  90  91
                                │
                 ▇▇▇▇▇▇▇▇▇▇▇▇▄▄▄│▁▁▁▁▁▁▁▁▄▄▄▄████▃▃▃▃▁▁▁▁███████████
-}
module ForecastHours where

import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy (ByteString)



data WeatherPoint = WeatherPoint {
  time          :: Double,
  temperature   :: Double, -- c
  precipitation :: Maybe Precipitation,
  cloudCover    :: Double, -- % 0-1
  humidity      :: Double  -- % 0-1
  }
  deriving (Show, Eq)

{- Notes on Parsing API Reponses

Reference: https://darksky.net/dev/docs/response

-- Required fields
  time

-- timeframe-conditional fields

* Certain fields concern a range (e.g. tempMin)
  but they are only present for a `daily` timeframe.


-- [1] About `precip*` Fields

* `type` only if `intensity` is > 0

* if timeframe is `minutely` then
  `intensity` only if `probability` is > 0

Therefore to determine precipitation existance we must check, in order:

  Is intensity present?
  Is intensity > 0?

or (since intensity depends on probability of value):

  Is probability > 0?
  Is intensity > 0?

Either way seems to achieve the same result, so pick as we wish.
-}
instance FromJSON WeatherPoint where
  parseJSON jsonData@(Object o) =
    WeatherPoint
    <$> o .: "time"
    <*> o .: "temperature"
    <*> parsedPrecipitation
    <*> o .: "humidity"
    <*> o .: "cloudCover"
    where
    parsedPrecipitation :: Parser (Maybe Precipitation)
    parsedPrecipitation = do -- [1]
      mpp <- o .:? "precipProbability" :: Parser (Maybe Double)
      mpn <- o .:? "precipIntensity"   :: Parser (Maybe Double)
      case (mpn, mpp) of
        (Just 0, _)      -> pure Nothing
        (_, Just 0)      -> pure Nothing
        (Just _, Just _) -> parseJSON jsonData
        _                -> pure Nothing

  parseJSON _              = mempty



data Precipitation = Precipitation {
  kind        :: PrecipitationKind,
  probability :: Double, -- % 0-1
  intensity   :: Double  -- Inches of liquid per hour
  }
  deriving (Show, Eq)

instance FromJSON Precipitation where
  parseJSON (Object o) =
    Precipitation
    <$> o .: "precipType"
    <*> o .: "precipProbability"
    <*> o .: "precipIntensity"

  parseJSON _          = mempty



data PrecipitationKind =
    Rain
  | Snow
  | Sleet
  deriving (Show, Eq)

instance FromJSON PrecipitationKind where
  parseJSON (String "rain")  = pure Rain
  parseJSON (String "snow")  = pure Snow
  parseJSON (String "sleet") = pure Sleet
  parseJSON _                = mempty




main :: [WeatherPoint] -> IO ()
main _ = undefined


demo :: IO ()
demo = do
  print (decode bs :: Maybe WeatherPoint)
  print (decode bs2 :: Maybe WeatherPoint)
  where
  bs = "{\"time\":1475697600,\"summary\":\"Clear\",\"icon\":\"clear-day\",\"precipIntensity\":0,\"precipProbability\":0,\"temperature\":64.72,\"apparentTemperature\":64.72,\"dewPoint\":46.38,\"humidity\":0.51,\"windSpeed\":6.63,\"windBearing\":287,\"visibility\":9.97,\"cloudCover\":0,\"pressure\":1018.96,\"ozone\":312.74}" :: ByteString
  bs2 = "{\"time\":1475697600,\"summary\":\"Clear\",\"icon\":\"clear-day\",\"precipIntensity\":0,\"precipProbability\":0.5,\"precipIntensity\":0.1,\"precipType\":\"snow\",\"temperature\":64.72,\"apparentTemperature\":64.72,\"dewPoint\":46.38,\"humidity\":0.51,\"windSpeed\":6.63,\"windBearing\":287,\"visibility\":9.97,\"cloudCover\":0,\"pressure\":1018.96,\"ozone\":312.74}"
