module Table where

test = putStr . render . align $ [
  Entry "Monday" [Dimension "temp" "40", Dimension "humidity" "0.43"],
  Entry "Tuesday" [Dimension "temp" "40", Dimension "humidity" "0.43"],
  Entry "Wednesday" [Dimension "temp" "40", Dimension "humidity" "0.43"]
  ]



-- Create a function that receives table data and returns a rendered table in the form of a string.

-- The table data will be a list of entries. An entry will be a dictionary with fields `id`, `dimensions`. `id` maps to a unique entry in the domain. `dimensions` maps to a list of dimensions. A dimension is a dictionary with fields `name`, `value`. `name` maps to a dimension and `value` is the content of that dimension, at that domain entry.

type RenderedTable = String

type TableData = [Entry]

data Entry =
  Entry {
    id         :: String,
    dimensions :: [Dimension]
  }
  deriving (Eq, Show)

data Dimension =
  Dimension {
    name  :: String,
    value :: String
  }
  deriving (Eq, Show)


type Row = [String]

-- Render will fold over the tableData to prduce a rendered table as string. There are two special cases. The first row containing column labels (domain entries) will need to be rendered once. The first column containing row labels (dimensions) will also need to be rendered once.

-- We need an intermediate representation which moves column oriented data into row oriented data given that text is line oriented.

render :: [Row] -> String
render = concatMap renderRow



renderRow :: Row -> String
renderRow row = foldl (++) "" row ++ "\n"

-- For example:
-- [["", "Monday", "Tuesday", "Wednesday", "Thursday"], ["Temp", "20", "21", "22", "30"]]


align :: TableData -> [Row]
align tableData = foldl f [[""]] (fmap equalizeWidths tableData)
  where
  f :: [Row] -> Entry -> [Row]
  f (headRow:dims) (Entry id dimEnts) =
    (headRow ++ [id]) : alignDimEntries dims dimEnts

  alignDimEntries :: [[String]] -> [Dimension] -> [[String]]
  alignDimEntries dims [] =
    dims
  alignDimEntries [] (Dimension name value:dimEnts) =
    [name, value] : alignDimEntries [] dimEnts
  alignDimEntries (dim:dims) (Dimension _ value:dimEnts) =
    (dim ++ [value]) : alignDimEntries dims dimEnts



equalizeWidths :: Entry -> Entry
equalizeWidths (Entry id dimensions) =
  Entry (padRight maxWidth id) (fmap padDimensionRight dimensions)
  where
  maxWidth = maximum $ length id : fmap dimensionLength dimensions
  dimensionLength (Dimension name value) =
    length value
  padDimensionRight (Dimension name value) =
    Dimension name (padRight maxWidth value)



padRight :: Int -> String -> String
padRight n s =
  s ++ nSpaces
  where
  nSpaces = concat $ take nEmpty spaces
  nEmpty = n - length s
  spaces = repeat " "
