module OpenBudget.Types.Database where

import           Data.Time (UTCTime)
import           OpenBudget.Types.Area
import           OpenBudget.Types.Document
import           OpenBudget.Types.Expense

data Database = Database
    { areas     :: [Area]
    , documents :: [Document]
    , expenses  :: [Expense]
    , version   :: UTCTime
    } deriving (Show, Read)
