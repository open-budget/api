{-# LANGUAGE OverloadedStrings #-}

module OpenBudget.Types.Area where

import           Data.Bson ((=:))
import qualified Data.Bson as Bson (Document)
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
    -- код регіону повинен  складатися лише з цифр
    | all isDigit id' = Just Area { areaId =  read id' :: Int, areaName = name }
    | otherwise       = Nothing
fromCSV _ = Nothing


-- | Конвертування регіону для зберігання в mongodb
toBSON :: Area          -- ^ внутрішнє представлення регіону
       -> Bson.Document -- ^ bson-представлення
toBSON a = [ "areaId" =: areaId a, "areaName" =: areaName a ]


-- конвертування статті розходів для представлення в веб api
-- toJSON
