{-# LANGUAGE OverloadedStrings #-}

module OpenBudget.Types.SpendItem where

import           Data.Bson    ((=:))
import qualified Data.Bson    as Bson (Document)
import qualified Text.CSV     as CSV
import qualified Text.Read.HT as HT

-- стаття росходів
data SpendItem = SpendItem

    -- рік
    -- область
    -- документ

    { code                 :: String       -- код ТКВ на кредитування місцевих бюджетів
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
    } deriving (Show, Read)


-- розбирання статті розходу за складовими частинами
-- (порядок полів визначений у законодавчих актах)
fromCSV :: CSV.Record -> Maybe SpendItem

-- todo: check code (should be 6 digits)
fromCSV (c:cn:gft:gfw:gfu:sft:ct:cw:cu:dt:db:ce:t:[]) =
    Just SpendItem
        { code                 = c
        , codeName             = cn
        , generalFundTotal     = d gft
        , generalFundWages     = d gfw
        , generalFundUtilities = d gfu
        , specialFundTotal     = d sft
        , consumptionTotal     = d ct
        , consumptionWages     = d cw
        , consumptionUtilities = d cu
        , developmentTotal     = d dt
        , developmentBudget    = d db
        , capitalExpenditures  = d ce
        , total                = read (f t) :: Double
        }
    where d i = HT.maybeRead (f i) :: Maybe Double
          f = filter (/= ',')
fromCSV _ = Nothing


-- конвертування статті розходів для зберігання в mongodb
toBSON :: SpendItem -> Bson.Document
toBSON s =
    [ "code"                 =: code s
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
