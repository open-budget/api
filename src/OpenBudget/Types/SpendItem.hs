{-# LANGUAGE OverloadedStrings #-}

module OpenBudget.Types.SpendItem where

import           Data.Bson    ((=:))
import qualified Data.Bson    as Bson (Document)
import           Data.Char    (isDigit)
import           Data.Text    (Text)
import           Data.UUID    (UUID, toString)
import           Data.UUID.V4
import qualified Text.CSV     as CSV
import qualified Text.Read.HT as HT


-- | Стаття витрат
data SpendItem = SpendItem

    { spendItemId          :: UUID         -- унікальний ідентифікатор статті витрат

    , spendItemAreaId      :: Int          -- внутрішній код регіону (0 - Україна, 1-24 - областi)
    , spendItemDocumentId  :: Text         -- посилання на id документу
    , spendItemYear        :: Int          -- звітний період

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
fromCSV :: CSV.Record           -- ^ результат парсингу CVS
        -> IO (Maybe SpendItem) -- ^ можлива стаття витрат
fromCSV (c:cn:gft:gfw:gfu:sft:ct:cw:cu:dt:db:ce:t:[])

    -- код повинен складатися не менш ніж з п'яти цифр
    | all isDigit c && length c > 5 = do
        uuid <- nextRandom
        return $ Just SpendItem
            { spendItemId          = uuid
            , spendItemAreaId      = 0
            , spendItemDocumentId  = ""
            , spendItemYear        = 2014
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
    | otherwise = return Nothing
        where md i = HT.maybeRead (removeCommas i) :: Maybe Double
              removeCommas = filter (/= ',')
fromCSV _ = return Nothing


-- | Конвертування статті витрат для зберігання в mongodb
toBSON :: SpendItem     -- ^ стаття витрат
       -> Bson.Document -- ^ bson-предстaвлення статті
toBSON s =
    [ "spendItemId"          =: toString (spendItemId s)
    , "spendItemAreaId"      =: spendItemAreaId s
    , "spendItemDocumentId"  =: spendItemDocumentId s
    , "spendItemYear"        =: spendItemYear s

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


-- | Оновлення внутрішнього коду регіону у створеній статті витрат
updateAreaId :: SpendItem -- ^ стаття розходів для оновлення
             -> Int       -- ^ новий код регіону
             -> SpendItem -- ^ оновлена стаття розходів
updateAreaId si aid = si { spendItemAreaId=aid }


-- | Оновлення ідентифікатора документу у створенній статті розходів
updateDocumentId :: SpendItem -- ^ стаття розходів для оновлення
                 -> Text      -- ^ новий ідентифікатор документу
                 -> SpendItem -- ^ оновлена стаття розходів
updateDocumentId si did = si { spendItemDocumentId=did }


-- | Оновлення звітного періоду у створенній статті розходів
updateYear :: SpendItem -- ^ стаття розходів для оновлення
           -> Int       -- ^ новий ідентифікатор документу
           -> SpendItem -- ^ оновлена стаття розходів
updateYear si y = si { spendItemYear=y }

