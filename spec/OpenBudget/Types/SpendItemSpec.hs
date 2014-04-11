module OpenBudget.Types.SpendItemSpec (main, spec) where

import           OpenBudget.Types.SpendItem
import           Test.Hspec
import           Text.CSV
import Data.Maybe

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "fromCSV" $ do
        --it "повинно корретно парсити корректний інпут" $ do
        --    let rawCsv = "010000,code name,\"4,744.100\",\"3,220.900\",103.200,0.000,,,,0.000,0.000,0.000,\"4,744.100"
        --        Right _csv = parseCSV "" rawCsv

        --    (fromCSV (head _csv)) `shouldBe`
        --      Just SpendItem
        --        { code = "010000"
        --        , codeName = "code name"
        --        , total = 4744.1
        --        , generalFundWages = Just 3220.9
        --        , generalFundUtilities = Just 103.2
        --        , generalFundTotal = Just 4744.1
        --        , specialFundTotal = Just 0.0
        --        , consumptionWages = Nothing
        --        , consumptionUtilities = Nothing
        --        , consumptionTotal = Nothing
        --        , developmentBudget = Just 0.0
        --        , capitalExpenditures = Just 0.0
        --        , developmentTotal = Just 0.0
        --        }

        it "повинно ігнорувати некорректний інпут" $ do
            let rawCsv = ",0112,a,a,a,a"
                Right _csv = parseCSV "" rawCsv

            (fromCSV (head _csv)) `shouldBe` Nothing
