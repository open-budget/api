module OpenBudget.Types.ExpenseSpec (spec) where

import           OpenBudget.Types.Expense
import           Test.Hspec
import           Text.CSV

spec :: Spec
spec =
    describe "fromCSV" $
        it "повинно ігнорувати некорректний інпут" $
            fromCSV (head _csv) `shouldBe` Nothing
            where rawCsv = ",0112,a,a,a,a"
                  Right _csv = parseCSV "" rawCsv
