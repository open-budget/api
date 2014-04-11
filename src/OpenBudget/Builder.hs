module OpenBudget.Builder where

import           Data.Maybe                 (fromJust, isJust)
import           Data.Time                  (getCurrentTime)
import           OpenBudget.Types.Area      hiding (fromCSV)
import qualified OpenBudget.Types.Area      as Area (fromCSV)
import           OpenBudget.Types.Database
import           OpenBudget.Types.Document  hiding (fromCSV)
import qualified OpenBudget.Types.Document  as Document (fromCSV)
import qualified Paths_open_budget_database as Paths
import           Text.CSV                   (Record, parseCSV)


newDatabase :: IO Database
newDatabase = do
    ct <- getCurrentTime
    a <- getAreas
    d <- getDocuments
    return Database { areas=a, documents=d, expenses=[], version=ct }


getSmthn :: FilePath -> (Record -> Maybe a) -> IO [a]
getSmthn f preprocess = do
    contents <- Paths.getDataFileName f >>= readFile
    case parseCSV f contents of
        Right _csv -> return $ map fromJust $ filter isJust $ map preprocess _csv
        Left _ -> return []


getAreas :: IO [Area]
getAreas = getSmthn "data/areas.csv" Area.fromCSV


getDocuments :: IO [Document]
getDocuments = getSmthn "data/documents.csv" Document.fromCSV
