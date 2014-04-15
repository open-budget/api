{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module OpenBudget.Types.Expense where

import           Data.Aeson                (ToJSON)
import           Data.Char                 (isDigit)
import           GHC.Generics
import           OpenBudget.Types.Document hiding (fromCSV)
import qualified Text.CSV                  as CSV
import qualified Text.Read.HT              as HT


-- | Стаття витрат
data Expense = Expense

    { expenseId            :: String       -- унікальний ідентифікатор статті витрат

    , expenseAreaId        :: Int          -- внутрішній код регіону (0 - Україна, 1-24 - областi)
    , expenseDocumentId    :: Int          -- посилання на id документу
    , expenseYear          :: Int          -- звітний період

    , code                 :: String       -- код ТКВ на кредитування місцевих бюджетів
    , codeName             :: String       -- найменування коду тимчасової класифікації видатків та кредитування місцевих бюджетів
    , total                :: Double       -- загальна сума

    -- видатки загального фонду
    , generalFundWages     :: Maybe Double -- оплата праці
    , generalFundUtilities :: Maybe Double -- комунальні послуги та енергоносії
    , generalFundTotal     :: Maybe Double -- всього

    -- видатки спеціального фонду
    , specialFundTotal     :: Maybe Double -- всього зі спеціального фонду

    -- -- споживання
    , consumptionWages     :: Maybe Double -- оплата праці
    , consumptionUtilities :: Maybe Double -- комунальні послуги та енергоносії
    , consumptionTotal     :: Maybe Double -- всього

    -- -- розвитку
    , developmentBudget    :: Maybe Double -- бюджет розвитку
    , capitalExpenditures  :: Maybe Double -- капітальні видатки
    , developmentTotal     :: Maybe Double -- всього
    } deriving (Show, Read, Eq, Generic)


-- конвертування статті розходів для представлення в веб api
instance ToJSON Expense


-- | Розбирання статті витрат за складовими частинами
--   (порядок полей визначений у законодавчих актах)
fromCSV :: CSV.Record    -- ^ результат парсингу CVS
        -> Maybe Expense -- ^ можлива стаття витрат
fromCSV (c:cn:gft:gfw:gfu:sft:ct:cw:cu:dt:db:ce:t:[])

    -- код повинен складатися не менш ніж з п'яти цифр
    | all isDigit c && length c > 5 =
        Just Expense
            { expenseId            = ""
            , expenseAreaId        = 0
            , expenseDocumentId    = 0
            , expenseYear          = 0
            , code                 = c
            , codeName             = cn
            , generalFundTotal     = md gft
            , generalFundWages     = md gfw
            , generalFundUtilities = md gfu
            , specialFundTotal     = md sft
            , consumptionTotal     = md ct
            , consumptionWages     = md cw
            , consumptionUtilities = md cu
            , developmentTotal     = md dt
            , developmentBudget    = md db
            , capitalExpenditures  = md ce
            , total                = read (removeCommas t) :: Double
            }
    | otherwise = Nothing
        where md i = HT.maybeRead (removeCommas i) :: Maybe Double
              removeCommas = filter (/= ',')
fromCSV _ = Nothing


-- | Оновлення унікального ідентифікатора статті витрат, побудованого з
--   року прийняття, коду регіону та коду статті витрат
updateItemId :: Expense -> Expense
updateItemId si = si { expenseId = code' }
    where [y, a, c] = fmap show [ expenseYear si, expenseAreaId si, read (code si) :: Int]
          code' = y ++ "-" ++ a ++ "-" ++ c


-- | Оновлення внутрішнього коду регіону у створеній статті витрат
updateAreaId :: Int     -- ^ новий код регіону
             -> Expense -- ^ стаття розходів для оновлення
             -> Expense -- ^ оновлена стаття розходів
updateAreaId aid si = si { expenseAreaId=aid }


-- | Оновлення ідентифікатора документу у створенній статті розходів
updateDocumentId :: Int     -- ^ новий ідентифікатор документу
                 -> Expense -- ^ стаття розходів для оновлення
                 -> Expense -- ^ оновлена стаття розходів
updateDocumentId did si = si { expenseDocumentId=did }


-- | Оновлення звітного періоду у створенній статті розходів
updateYear :: Int     -- ^ новий ідентифікатор документу
           -> Expense -- ^ стаття розходів для оновлення
           -> Expense -- ^ оновлена стаття розходів
updateYear y si = si { expenseYear=y }


-- | Прив'язка статті витрат до конкретного документу та оновлення регіону та періоду
linkToDocument :: Document -- ^ документ, до якого буде прив'язана стаття розходів
               -> Expense  -- ^ первинна стаття розходів
               -> Expense  -- ^ прив'язана стаття розходів
linkToDocument doc = updateItemId . updateYear (documentYear doc) . updateDocumentId (read (documentId doc) :: Int) . updateAreaId (documentArea doc)
