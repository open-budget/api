{-# LANGUAGE OverloadedStrings #-}

module OpenBudget.Types.Area where

import           Data.Aeson     (ToJSON, object, toJSON, (.=))
import           Data.Char      (isDigit, toLower)
import           Data.List      (isInfixOf)
import           Data.Text.Lazy (unpack)
import qualified Text.CSV       as CSV
import           Web.Scotty     (Param)


-- | Регіон (суб'єкт) бюджетування
data Area = Area
    { areaId   :: Int     -- внутрішній код регіону (0 - Україна, 1-24 - областi)
    , areaName :: String  -- назва регіону
    } deriving (Show, Read, Eq)

-- конвертування регіону для представлення в веб api
instance ToJSON Area where
    toJSON (Area id' name) = object
        [ "id"   .= id'
        , "name" .= name
        ]


-- | Конвертація результату парсингу CSV у внутрішнє представлення
fromCSV :: CSV.Record -- ^ результат парсингу CVS
        -> Maybe Area -- ^ можлива стаття розходів
fromCSV (id':name:[])
    | all isDigit id' = Just Area { areaId =  read id' :: Int, areaName = name }
    | otherwise       = Nothing
fromCSV _ = Nothing


-- | Створення виборки серед регіонів по заданим параметрам. Параметри беруться
--   з рядка запиту (http query string). При наявності декілька ключів у параметра
--   виборка відфільтрованих регіонів звужується кожною новою фільтрацією.
select :: [Param] -- ^ перелік кортежів параметрів запиту у вигляді (ключ, значення)
       -> [Area]  -- ^ первинний перелік регіонів
       -> [Area]  -- ^ регіони, шо задовольняють введений параметрам запиту
select [] areas = areas
select _  []    = []
select ((key',value'):params) areas =

    case key of
        "id"     -> select params (filter (\a -> (read value :: Int) == areaId a) areas)
        "search" -> select params (filter (\a -> map toLower value `isInfixOf` map toLower (areaName a)) areas)

        -- скiпаємо будь-які незнані ключі
        _        -> select params areas

        where (key, value) = (unpack key', unpack value')
