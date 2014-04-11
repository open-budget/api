module OpenBudget.Types.ExpenseSpec (spec) where

import           OpenBudget.Types.Expense
import           Test.Hspec
import           Text.CSV

spec :: Spec
spec =
    describe "fromCSV" $
        it "повинно ігнорувати некорректний інпут" $ do
            let rawCsv = ",0112,a,a,a,a"
                Right _csv = parseCSV "" rawCsv

            readFile "data/areas.csv" >>= print

            fromCSV (head _csv) `shouldBe` Nothing
