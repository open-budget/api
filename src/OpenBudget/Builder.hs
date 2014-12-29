{-# LANGUAGE OverloadedStrings #-}

module OpenBudget.Builder where

import           Data.Maybe                 (fromJust, isJust)
import           Data.Time                  (getCurrentTime)
import           OpenBudget.Types.Area      hiding (fromCSV)
import qualified OpenBudget.Types.Area      as Area (fromCSV)
import           OpenBudget.Types.Database
import           OpenBudget.Types.Document  hiding (fromCSV)
import qualified OpenBudget.Types.Document  as Document (fromCSV)
import           OpenBudget.Types.Expense   hiding (fromCSV)
import qualified OpenBudget.Types.Expense   as Expense (fromCSV)
import qualified Paths_api                  as Paths
import           Text.CSV                   (Record, parseCSV)


newDatabase :: IO Database
newDatabase = do
    ct <- getCurrentTime
    a <- getAreas
    d <- getDocuments
    e <- getExpenses d
    return Database { areas=a, documents=d, expenses=e, version=ct }


getSmthn :: FilePath -> (Record -> Maybe a) -> IO [a]
getSmthn f preprocess = do
    contents <- Paths.getDataFileName f >>= readFile
    case parseCSV f contents of
        Right _csv -> return $ map fromJust $ filter isJust $ map preprocess _csv
        Left _ -> return []


getAreas :: IO [Area]
getAreas = getSmthn "data/areas/index.csv" Area.fromCSV


getDocuments :: IO [Document]
getDocuments = getSmthn "data/expenses/index.csv" Document.fromCSV


getExpenses :: [Document] -> IO [Expense]
getExpenses docs =
    fmap concat (mapM getExpensesFromDoc docs)
    where
        getExpensesFromDoc :: Document -> IO [Expense]
        getExpensesFromDoc doc =
            fmap (map (linkToDocument doc)) $ getSmthn docpath Expense.fromCSV
                where docpath = "data/expenses/" ++ show(documentId doc) ++ ".csv"
