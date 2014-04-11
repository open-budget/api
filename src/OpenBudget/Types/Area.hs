{-# LANGUAGE OverloadedStrings #-}

module OpenBudget.Types.Area where

import           Data.Char (isDigit)
import qualified Text.CSV  as CSV


-- | Регіон (суб'єкт) бюджетування
data Area = Area
    { areaId   :: Int     -- внутрішній код регіону (0 - Україна, 1-24 - областi)
    , areaName :: String  -- назва регіону
    } deriving (Show, Read, Eq)


-- | Конвертація результату парсингу CSV у внутрішнє представлення
fromCSV :: CSV.Record -- ^ результат парсингу CVS
        -> Maybe Area -- ^ можлива стаття розходів
fromCSV (id':name:[])
    | all isDigit id' = Just Area { areaId =  read id' :: Int, areaName = name }
    | otherwise       = Nothing
fromCSV _ = Nothing


-- конвертування регіону для представлення в веб api
-- toJSON
