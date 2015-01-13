{-# LANGUAGE OverloadedStrings #-}

module OpenBudget.Builder where

import           Control.Applicative       ((<$>))
import           Data.Maybe                (fromJust, isJust, fromMaybe)
import           Data.Time                 (getCurrentTime)
import           OpenBudget.Types.Area     hiding (fromCSV)
import qualified OpenBudget.Types.Area     as Area (fromCSV)
import           OpenBudget.Types.Database
import           OpenBudget.Types.Document hiding (fromCSV)
import qualified OpenBudget.Types.Document as Document (fromCSV)
import           OpenBudget.Types.Expense  hiding (fromCSV)
import qualified OpenBudget.Types.Expense  as Expense (fromCSV)
import qualified Paths_api                 as Paths
import           System.Environment        (lookupEnv)
import           Text.CSV                  (Record, parseCSV)


newDatabase :: IO Database
newDatabase = do
    ct <- getCurrentTime
    a <- getAreas
    d <- getDocuments
    e <- getExpenses d
    return Database { areas=a, documents=d, expenses=e, version=ct }


getSmthn :: FilePath -> (Record -> Maybe a) -> IO [a]
getSmthn f preprocess = do
    dataPath <- fromMaybe standardPath <$> lookupEnv standardEnvKey
    contents <- Paths.getDataFileName (dataPath ++ "/" ++ f) >>= readFile
    case parseCSV f contents of
        Right _csv -> return $ map fromJust $ filter isJust $ map preprocess _csv
        Left _ -> return []
    where
        standardPath = "/usr/local/open-budget/data"
        standardEnvKey = "API_DATA_PATH"


getAreas :: IO [Area]
getAreas = getSmthn "areas/index.csv" Area.fromCSV


getDocuments :: IO [Document]
getDocuments = getSmthn "expenses/index.csv" Document.fromCSV


getExpenses :: [Document] -> IO [Expense]
getExpenses docs =
    fmap concat (mapM getExpensesFromDoc docs)


getExpensesFromDoc :: Document -> IO [Expense]
getExpensesFromDoc doc =
    map (linkToDocument doc) <$> getSmthn docpath Expense.fromCSV
        where docpath = "expenses/" ++ show(documentId doc) ++ ".csv"
