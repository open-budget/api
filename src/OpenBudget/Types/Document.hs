{-# LANGUAGE OverloadedStrings #-}

module OpenBudget.Types.Document where

import           Data.Bson ((=:))
import qualified Data.Bson as BSON (Document)
import           Data.UUID (UUID, toString)


-- | Документ, який фіксує обсяги та шляхи бюджетування
data Document = Document
    { documentId   :: UUID    -- унікальний ідентифікатор документу
    , documentName :: String  -- назва документу
    , documentYear :: Int     -- період бюджетування
    , documentLink :: String  -- посилання на джерело документу
    , documentType :: Int     -- тип документу
    } deriving (Show, Read, Eq)


-- | Конвертування документу для зберігання в mongodb
toBSON :: Document      -- ^ внутрішнє представлення документу
       -> BSON.Document -- ^ bson-представлення
toBSON d =
    [ "documentId"   =: toString (documentId d)
    , "documentName" =: documentName d
    , "documentYear" =: documentYear d
    , "documentLink" =: documentLink d
    , "documentType" =: documentType d
    ]
