module Main where

import qualified OpenBudget.Types.SpendItem as SpendItem
import qualified Text.CSV as CSV
import           Data.Maybe (fromJust)

main :: IO ()
main = do
    let filename = "spec/fixtures/file.csv"
    filecontents <- readFile filename

    -- persing csv
    let result = CSV.parseCSV filename filecontents
    case result of
        Left parseError -> print parseError
        Right _csv -> mapM_ (print . SpendItem.toBSON . fromJust . SpendItem.fromCSV) _csv
