module Main where

import           Data.Maybe                 (fromJust, isJust)
import qualified OpenBudget.Types.SpendItem as SpendItem
import qualified Text.CSV                   as CSV

main :: IO ()
main = do
    let filename = "spec/fixtures/file.csv"
    filecontents <- readFile filename

    -- persing csv
    let result = CSV.parseCSV filename filecontents
    case result of
        Left parseError -> print parseError
        Right _csv -> mapM_ (print . fromJust) $ filter isJust $  map SpendItem.fromCSV _csv
