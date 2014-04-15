{-# LANGUAGE OverloadedStrings #-}

module Main where

import           OpenBudget.Builder
import           OpenBudget.Types.Database
import           Web.Scotty

main :: IO ()
main = do
    db <- newDatabase

    scotty 3000 $ do

        get "/areas.json" $
            json (areas db)

        get "/documents.json" $
            json (documents db)

        get "/expenses.json" $
            json (expenses db)
