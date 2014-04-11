module Main where

import           Data.Maybe               (fromJust, isJust)
import qualified OpenBudget.Types.Expense as Expense
import qualified Text.CSV                 as CSV

main :: IO ()
main = do
    let filename = "spec/fixtures/file.csv"
    filecontents <- readFile filename

    -- persing csv
    let result = CSV.parseCSV filename filecontents
    case result of
        Left parseError -> print parseError
        Right _csv -> mapM_ (print . fromJust) $ filter isJust $  map Expense.fromCSV _csv
