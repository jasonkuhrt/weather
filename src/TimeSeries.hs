module TimeSeries where

-- This module is concerned with rendering time-series compatible data into a string that visualizes via graph techniques.



type DataPoint = (Int, [Observation]) -- time, observations
type Observation = (String, Float) -- label, value



-- Development --

mockData =
  [
    (1458014400, [
      ("temperature", 1),
      ("humidity", 1),
      ("cloud cover", 1)
    ]),
    (1458100800, [
      ("temperature", 2),
      ("humidity", 2),
      ("cloud cover", 2)
    ]),
    (1458187200, [
      ("temperature", 3),
      ("humidity", 3),
      ("cloud cover", 3)
    ])
  ]



testRenderValue = renderValue 4 $ snd . head . snd . head $ mockData
testRenderDataPoints = putStr $ str ++ "\n"
  where
  str = renderDataPoints mockData



-- Module --

renderDataPoints :: [DataPoint] -> String
renderDataPoints dps =
  unlines [columnHeadsStr, observationStr]
  where
  observationStr = renderRow " " (label : map (renderValue columnWidth) values)
  columnHeadsStr = renderRow " " (gap rowHeadsColumnWidth : columnHeads)
  columnWidth = calcColumnWidth columnHeads
  columnHeads = map (show . fst) dps
  rowHeadsColumnWidth = calcColumnWidth rowHeads
  rowHeads = (map fst . snd . head) dps
  values = map (snd . head . snd) dps
  label = (fst . head . snd . head) dps

renderValue :: Int -> Float -> String
renderValue width value =
  padRight width (show value)

renderRow border = concatMap (++ border)

calcColumnWidth = maximum . map length



-- Helpers --

padRight :: Int -> String -> String
padRight n s =
  s ++ gap nEmpty
  where
  nEmpty = n - length s

gap :: Int -> String
gap n = (concat . take n) spaces
  where
    spaces = repeat " "
