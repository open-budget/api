{-# LANGUAGE OverloadedStrings #-}

module OpenBudget.Types.Document where

import           Data.Char (isDigit)
import qualified Text.CSV  as CSV

-- | Документ, який фіксує обсяги та шляхи бюджетування
data Document = Document
    { documentId          :: String  -- унікальний ідентифікатор документу
    , documentYear        :: Int     -- період
    , documentArea        :: Int     -- регіон
    , documentName        :: String  -- назва документу
    , documentDescription :: String  -- додаткові дані
    , documentLink        :: String  -- посилання на джерело документу
    , documentType        :: Int     -- тип документу
    , documentFilename    :: String
    } deriving (Show, Read, Eq)

fromCSV :: CSV.Record
        -> Maybe Document
fromCSV (id':y:a:n:d:l:t:f:[])
    | all isDigit id' = Just (Document id' (i y) (i a) n d l (i t) f)
    | otherwise       = Nothing
    where i x = read x :: Int
fromCSV _ = Nothing
