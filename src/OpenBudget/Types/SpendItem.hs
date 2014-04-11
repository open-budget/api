{-# LANGUAGE OverloadedStrings #-}

module OpenBudget.Types.SpendItem where

import           Data.Bson    ((=:))
import qualified Data.Bson    as Bson (Document)
import           Data.Char    (isDigit)
import           Data.Text    (Text)
import qualified Text.CSV     as CSV
import qualified Text.Read.HT as HT

-- | Стаття росходів
data SpendItem = SpendItem

    { areaId               :: Int          -- внутрішній код регіону (0 - Україна, 1-24 - областi)
    , documentId           :: Text         -- посилання на id документу
    , year                 :: Int          -- звітний період

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


-- | Розбирання статті розходу за складовими частинами
--   (порядок полів визначений у законодавчих актах)
fromCSV :: CSV.Record      -- ^ результат парсингу CVS
        -> Maybe SpendItem -- ^ можлива стаття розходів
fromCSV (c:cn:gft:gfw:gfu:sft:ct:cw:cu:dt:db:ce:t:[])

    -- код повинен складатися не менш ніж з п'яти цифр
    | all isDigit c && length c > 5 =
        Just SpendItem
            { areaId               = 0
            , documentId           = ""
            , year                 = 2014
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


-- | Конвертування статті розходів для зберігання в mongodb
toBSON :: SpendItem     -- ^ стаття розходів
       -> Bson.Document -- ^ bson-предстaвлення статті
toBSON s =
    [ "areaId"               =: areaId s
    , "documentId"           =: documentId s
    , "year"                 =: year s

    , "code"                 =: code s
    , "codeName"             =: codeName s
    , "total"                =: total s

    , "generalFundWages"     =: generalFundWages s
    , "generalFundUtilities" =: generalFundUtilities s
    , "generalFundTotal"     =: generalFundTotal s

    , "specialFundTotal"     =: specialFundTotal s
    , "consumptionWages"     =: consumptionWages s
    , "consumptionUtilities" =: consumptionUtilities s
    , "consumptionTotal"     =: consumptionTotal s
    , "developmentBudget"    =: developmentBudget s
    , "capitalExpenditures"  =: capitalExpenditures s
    , "developmentTotal"     =: developmentTotal s
    ]

-- конвертування статті розходів для представлення в веб api
-- toJSON


-- | Оновлення внутрішнього коду регіону у створеній статті розходів
updateAreaId :: SpendItem -- ^ стаття розходів для оновлення
             -> Int       -- ^ новий код регіону
             -> SpendItem -- ^ оновлена стаття розходів
updateAreaId si aid = si { areaId=aid }


-- | Оновлення ідентифікатора документу у створенній статті розходів
updateDocumentId :: SpendItem -- ^ стаття розходів для оновлення
                 -> Text      -- ^ новий ідентифікатор документу
                 -> SpendItem -- ^ оновлена стаття розходів
updateDocumentId si did = si { documentId=did }


-- | Оновлення звітного періоду у створенній статті розходів
updateYear :: SpendItem -- ^ стаття розходів для оновлення
           -> Int       -- ^ новий ідентифікатор документу
           -> SpendItem -- ^ оновлена стаття розходів
updateYear si y = si { year=y }

