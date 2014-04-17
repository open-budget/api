{-# LANGUAGE OverloadedStrings #-}

module OpenBudget.Types.Document where

import           Data.Aeson     (ToJSON, object, toJSON, (.=))
import           Data.Char      (isDigit, toLower)
import           Data.List      (isInfixOf)
import           Data.Text.Lazy (unpack)
import qualified Text.CSV       as CSV
import           Web.Scotty     (Param)


-- | Документ, який фіксує обсяги та шляхи бюджетування
data Document = Document
    { documentId          :: Int     -- унікальний ідентифікатор документу
    , documentYear        :: Int     -- період
    , documentArea        :: Int     -- регіон
    , documentName        :: String  -- назва документу
    , documentDescription :: String  -- додаткові дані
    , documentLink        :: String  -- посилання на джерело документу
    , documentType        :: String  -- тип документу
    , documentFilename    :: String
    } deriving (Show, Read, Eq)


-- | Конвертування документу для представлення в веб api
instance ToJSON Document where
    toJSON (Document id_ year area name desc link type_ _) = object
        [ "id"          .= id_
        , "year"        .= year
        , "area_id"     .= area
        , "name"        .= name
        , "description" .= desc
        , "link"        .= link
        , "type"        .= type_
        ]


-- | Конвертація результату парсингу CSV у внутрішнє представлення
fromCSV :: CSV.Record     -- ^ результат парсингу CVS
        -> Maybe Document -- ^ можливий документ
fromCSV (id':y:a:n:d:l:t:f:[])
    | all isDigit id' = Just (Document (i id') (i y) (i a) n d l t f)
    | otherwise       = Nothing
    where i x = read x :: Int
fromCSV _ = Nothing


-- | Створення виборки серед документів по заданим параметрам. Параметри беруться
--   з рядка запиту (http query string). При наявності декілька ключів у параметра
--   виборка відфільтрованих документів звужується кожною новою фільтрацією.
select :: [Param]    -- ^ перелік кортежів параметрів запиту у вигляді (ключ, значення)
       -> [Document] -- ^ первинний перелік документів
       -> [Document] -- ^ документи, шо задовольняють введений параметрам запиту
select [] docs = docs
select _  []   = []
select ((key',value'):params) docs =
    case key of
        "area_id" -> select params (sameInt docs documentArea)
        "year"    -> select params (sameInt docs documentYear)
        "id"      -> select params (sameInt docs documentId)
        "search"  -> select params (filter (\doc -> map toLower value `isInfixOf` map toLower (documentName doc)) docs)
        _         -> select params docs -- скiпаємо будь-які незнані ключі

        where sameInt docs' field =
                -- в разі передачі списку значень замість одного, шукаємо
                -- співпадіння кожного з введеного переліку
                if "," `isInfixOf` value
                    then filter (\d -> [field d] `isInfixOf` valueList) docs'
                    else filter (\d -> (read value :: Int) == field d) docs'

              valueList = read ("[" ++ value ++ "]") :: [Int]
              (key, value) = (unpack key', unpack value')
