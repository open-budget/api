module OpenBudget.Types.SpendItemSpec (main, spec) where

import           OpenBudget.Types.SpendItem
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "fromCSV" $
        it "should be true" $
            True `shouldBe` not False
