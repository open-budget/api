{-# LANGUAGE OverloadedStrings #-}

module OpenBudget.Types.Expense where

import           Data.Aeson                (ToJSON, object, toJSON, (.=))
import           Data.Char                 (isDigit, toLower)
import           Data.List                 (isInfixOf)
import           Data.Text.Lazy            (unpack)
import           OpenBudget.Types.Document hiding (fromCSV, select)
import qualified Text.CSV                  as CSV
import qualified Text.Read.HT              as HT
import           Web.Scotty                (Param)


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
    } deriving (Show, Read, Eq)


-- конвертування статті розходів для представлення в веб api
instance ToJSON Expense where
    toJSON (Expense eid eaid edid ey c cn t gfw gfu gft sft cw cu ct db ce dt) = object
        [ "id"                     .= eid
        , "area_id"                .= eaid
        , "document_id"            .= edid
        , "year"                   .= ey
        , "code"                   .= c
        , "code_name"              .= cn
        , "total"                  .= t
        , "general_fund_wages"     .= gfw
        , "general_fund_utilities" .= gfu
        , "general_fund_total"     .= gft
        , "special_fund_total"     .= sft
        , "consumption_wages"      .= cw
        , "consumption_utilities"  .= cu
        , "consumption_total"      .= ct
        , "development_budget"     .= db
        , "capital_expenditures"   .= ce
        , "development_total"      .= dt
        ]


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


-- | Прив'язка статті витрат до конкретного документу та оновлення регіону та періоду
linkToDocument :: Document -- ^ документ, до якого буде прив'язана стаття розходів
               -> Expense  -- ^ первинна стаття розходів
               -> Expense  -- ^ прив'язана стаття розходів
linkToDocument doc = updateItemId . updateYear (documentYear doc) . updateDocumentId (documentId doc) . updateAreaId (documentArea doc)
    where
          -- оновлення звітного періоду у створенній статті розходів
          updateYear y si = si { expenseYear=y }

          -- оновлення ідентифікатора документу у створенній статті розходів
          updateDocumentId did si = si { expenseDocumentId=did }

          -- оновлення внутрішнього коду регіону у створеній статті витрат
          updateAreaId aid si = si { expenseAreaId=aid }

          -- оновлення унікального ідентифікатора статті витрат, побудованого з
          updateItemId si = si { expenseId = code' }
              where [y', a, c] = fmap show [ expenseYear si, expenseAreaId si, read (code si) :: Int]
                    code' = y' ++ "-" ++ a ++ "-" ++ c


-- | Створення виборки серед видатків по заданим параметрам. Параметри беруться
--   з рядка запиту (http query string). При наявності декілька ключів у параметра
--   виборка відфільтрованих видатків звужується кожною новою фільтрацією.
select :: [Param]   -- ^ перелік кортежів параметрів запиту у вигляді (ключ, значення)
       -> [Expense] -- ^ первинний перелік видатків
       -> [Expense] -- ^ видатки, шо задовольняють введений параметрам запиту
select [] expenses = expenses
select _  []       = []
select ((key',value'):params) expenses =
    let filtered = case key of
                       "area_id"     -> sameInt expenses expenseAreaId
                       "year"        -> sameInt expenses expenseYear
                       "document_id" -> sameInt expenses expenseDocumentId
                       "id"          -> filter (\e -> value == expenseId e) expenses
                       "search"      -> filter (\e -> map toLower value `isInfixOf` map toLower (codeName e)) expenses
                       "code"        -> if "," `isInfixOf` value
                                            -- обробляємо перелік значень
                                            then filter (\e -> [read (code e) :: Int] `isInfixOf` valueList) expenses
                                            else filter (\e -> value == code e) expenses
                       _             -> expenses -- скiпаємо будь-які незнані ключі

    -- продовжуємо пошук у вже відфильтрованих результатах
    in select params filtered

        where sameInt exps field =
                  -- в разі передачі списку значень замість одного, шукаємо
                  -- співпадіння кожного з введеного переліку
                  if "," `isInfixOf` value
                      then filter (\d -> [field d] `isInfixOf` valueList) exps
                      else filter (\e -> (read value :: Int) == field e) exps

              valueList = read ("[" ++ value ++ "]") :: [Int]
              (key, value) = (unpack key', unpack value')
