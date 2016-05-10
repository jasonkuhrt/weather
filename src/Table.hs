{-# LANGUAGE DeriveFunctor #-}

module Table where

import Data.List



-- Custom Types --

newtype Cell a = Cell a deriving (Eq, Show, Functor)
type Row = [Cell String]
type Table = [Row]
type Column = [Cell String]



renderTable :: Table -> String
renderTable = unlines . fmap renderRow . sizeColumns
  where
  sizeColumns = mapColumns sizeColumn
  renderRow = intercalate columnBorder . fmap cellValue
  columnBorder = " "



-- Table Helpers --

sizeColumn :: Column -> Column
sizeColumn cells = (fmap . fmap) (setWidth n) cells
  where
  n = maximum . fmap (length . cellValue) $ cells

mapColumns :: (Column -> Column) -> Table -> Table
mapColumns f = transpose . fmap f . transpose

cellValue (Cell x) = x

makeTable :: [[String]] -> Table
makeTable = fmap . fmap $ Cell



-- String Helpers --

setWidth :: Int -> String -> String
setWidth n s =
  s ++ gap nEmpty
  where
  nEmpty = n - length s

gap :: Int -> String
gap n = (concat . take n) spaces
  where
    spaces = repeat " "



-- Development --

t = putStr $ "\n" ++ renderTable mockData ++ "\n"

mockData :: Table
mockData = makeTable
  [
    ["", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday"],
    ["Temperature", "3", "5", "10", "11", "9"],
    ["Humidity", "10", "15", "50", "0", "2"],
    ["Cloud Cover", "33", "22", "11", "90", "100"]
  ]
