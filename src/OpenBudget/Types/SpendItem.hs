{-# LANGUAGE OverloadedStrings #-}

module OpenBudget.Types.Expense where

import           Data.Char    (isDigit)
import           Data.Text    (Text)
import qualified Text.CSV     as CSV
import qualified Text.Read.HT as HT


-- | Стаття витрат
data Expense = Expense

    { expenseId            :: String       -- унікальний ідентифікатор статті витрат

    , expenseAreaId        :: Int          -- внутрішній код регіону (0 - Україна, 1-24 - областi)
    , expenseDocumentId    :: Text         -- посилання на id документу
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
    } deriving (Show, Read, Eq)


-- | Розбирання статті витрат за складовими частинами
--   (порядок полей визначений у законодавчих актах)
fromCSV :: CSV.Record    -- ^ результат парсингу CVS
        -> Maybe Expense -- ^ можлива стаття витрат
fromCSV (c:cn:gft:gfw:gfu:sft:ct:cw:cu:dt:db:ce:t:[])

    -- код повинен складатися не менш ніж з п'яти цифр
    | all isDigit c && length c > 5 =
        Just Expense
            { expenseId          = ""
            , expenseAreaId      = 0
            , expenseDocumentId  = ""
            , expenseYear        = 2014
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


-- конвертування статті розходів для представлення в веб api
-- toJSON


-- | Оновлення унікального ідентифікатора статті витрат, побудованого з
--   року прийняття, коду регіону та коду статті витрат
updateItemId :: Expense -> Expense
updateItemId si = si { expenseId = code' }
    where [y, a, c] = fmap show [ expenseYear si, expenseAreaId si, read (code si) :: Int]
          code' = y ++ "-" ++ a ++ "-" ++ c


-- | Оновлення внутрішнього коду регіону у створеній статті витрат
updateAreaId :: Expense -- ^ стаття розходів для оновлення
             -> Int     -- ^ новий код регіону
             -> Expense -- ^ оновлена стаття розходів
updateAreaId si aid = si { expenseAreaId=aid }


-- | Оновлення ідентифікатора документу у створенній статті розходів
updateDocumentId :: Expense -- ^ стаття розходів для оновлення
                 -> Text    -- ^ новий ідентифікатор документу
                 -> Expense -- ^ оновлена стаття розходів
updateDocumentId si did = si { expenseDocumentId=did }


-- | Оновлення звітного періоду у створенній статті розходів
updateYear :: Expense -- ^ стаття розходів для оновлення
           -> Int     -- ^ новий ідентифікатор документу
           -> Expense -- ^ оновлена стаття розходів
updateYear si y = si { expenseYear=y }

