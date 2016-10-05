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



{- Notes on Parsing API Reponses

Reference: https://darksky.net/dev/docs/response

-- Required fields
  time

-- timeframe-conditional fields

* Certain fields concern a range (e.g. tempMin)
  but they are only present for a `daily` timeframe.


-- About `precip*` Fields

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



data WeatherPoint = WeatherPoint {
  time          :: Double,
  temperature   :: Double, -- c
  precipitation :: Maybe Precipitation,
  cloudCover    :: Double, -- % 0-1
  humidity      :: Double  -- % 0-1
  }
  deriving (Show, Eq)

data Precipitation = Precipitation {
  kind        :: PrecipitationKind,
  probability :: Double, -- % 0-1
  intensity   :: Double  -- Inches of liquid per hour
  }
  deriving (Show, Eq)

data PrecipitationKind =
    Sleet -- e.g. freezing rain | ice pellets | wintery mix
  | Rain
  | Snow
  deriving (Show, Eq)




main :: [WeatherPoint] -> IO ()
main _ = undefined
