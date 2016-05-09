module Table where

import Data.List

type Cell = String
type Row = [Cell]
type Table = [Row]
type Column = [Cell]



renderTable :: Table -> String
renderTable = concatMap renderRow . sizeColumns
  where
  renderRow = intercalate " " . (++ ["\n"])
  sizeColumns = mapColumns sizeColumn



-- Table Helpers --

sizeColumn :: Column -> Column
sizeColumn cells = map (sizeWidth n) cells
  where
  n = maximum . (map length) $ cells

mapColumns :: (Column -> Column) -> Table -> Table
mapColumns f = transpose . map f . transpose

mapCells :: (Cell -> Cell) -> Table -> Table
mapCells f = mapColumns (map f)



-- String Helpers --

sizeWidth :: Int -> String -> String
sizeWidth n s =
  s ++ gap nEmpty
  where
  nEmpty = n - length s

gap :: Int -> String
gap n = (concat . take n) spaces
  where
    spaces = repeat " "



-- Development --

t = putStr $ "\n" ++ renderTable mockData ++ "\n"

mockData =
  [
    ["", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday"],
    ["Temperature", "3", "5", "10", "11", "9"],
    ["Humidity", "10", "15", "50", "0", "2"],
    ["Cloud Cover", "33", "22", "11", "90", "100"]
  ]
