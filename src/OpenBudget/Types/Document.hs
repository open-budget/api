{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module OpenBudget.Types.Document where

import           Data.Aeson   (ToJSON)
import           Data.Char    (isDigit)
import           GHC.Generics
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
    } deriving (Show, Read, Eq, Generic)


-- | Конвертування документу для представлення в веб api
instance ToJSON Document


-- | Конвертація результату парсингу CSV у внутрішнє представлення
fromCSV :: CSV.Record     -- ^ результат парсингу CVS
        -> Maybe Document -- ^ можливий документ
fromCSV (id':y:a:n:d:l:t:f:[])
    | all isDigit id' = Just (Document id' (i y) (i a) n d l t f)
    | otherwise       = Nothing
    where i x = read x :: Int
fromCSV _ = Nothing
