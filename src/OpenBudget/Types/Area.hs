{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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
    toJSON Area{..} = object
        [ "id"   .= areaId
        , "name" .= areaName
        ]


-- | Конвертація результату парсингу CSV у внутрішнє представлення
fromCSV :: CSV.Record -- ^ результат парсингу CVS
        -> Maybe Area -- ^ можливий суб’єкт бюджетування
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
    let filtered = case key of
                       "search" -> filter (\a -> map toLower value `isInfixOf` map toLower (areaName a)) areas
                       "id"     -> -- в разі передачі списку значень замість одного, шукаємо
                                   -- співпадіння кожного з введеного переліку
                                   if "," `isInfixOf` value
                                       then filter (\a -> [areaId a] `isInfixOf` valueList) areas
                                       else filter (\a -> (read value :: Int) == areaId a) areas
                       _        -> areas -- скiпаємо будь-які незнані ключі

    -- продовжуємо пошук у вже відфильтрованих результатах
    in select params filtered
        where valueList = read ("[" ++ value ++ "]") :: [Int]
              (key, value) = (unpack key', unpack value')
