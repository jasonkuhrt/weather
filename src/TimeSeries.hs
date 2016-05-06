module TimeSeries where

import Data.List

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
renderDataPoints dps = renderedTable
  where
  renderedTable = renderTable contentRows
  renderTable = unlines . map (renderRow " ")
  contentRows = columnHeads : observations
  columnHeads = gap rowHeadsColumnWidth : map (show . fst) dps
  rowHeadsColumnWidth = calcColumnWidth rowHeads
  rowHeads = (map fst . snd . head) dps
  observations = gatherObservations (map snd dps)
  observation = label : map (renderValue columnWidth) values
  values = map (snd . head . snd) dps
  label = (fst . head . snd . head) dps
  columnWidth = calcColumnWidth columnHeads
  gatherObservations dpObs = gathered
    where
    gathered = transpose (labels : values)
    values = map (map (renderValue columnWidth . snd)) dpObs
    labels = map (padRight rowHeadsColumnWidth . fst) . head $ dpObs

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