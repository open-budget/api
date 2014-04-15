{-# LANGUAGE OverloadedStrings #-}

module OpenBudget.Types.Document where

import           Data.Aeson   (ToJSON, toJSON, object, (.=))
import           Data.Char    (isDigit)
import qualified Text.CSV     as CSV


-- | Документ, який фіксує обсяги та шляхи бюджетування
data Document = Document
    { documentId          :: String  -- унікальний ідентифікатор документу
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
        , "area"        .= area
        , "name"        .= name
        , "description" .= desc
        , "link"        .= link
        , "type"        .= type_
        ]


-- | Конвертація результату парсингу CSV у внутрішнє представлення
fromCSV :: CSV.Record     -- ^ результат парсингу CVS
        -> Maybe Document -- ^ можливий документ
fromCSV (id':y:a:n:d:l:t:f:[])
    | all isDigit id' = Just (Document id' (i y) (i a) n d l t f)
    | otherwise       = Nothing
    where i x = read x :: Int
fromCSV _ = Nothing
