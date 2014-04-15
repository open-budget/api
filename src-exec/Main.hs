{-# LANGUAGE OverloadedStrings #-}

module Main where

import           OpenBudget.Builder
import           OpenBudget.Types.Database
import           Web.Scotty
import           Paths_open_budget_database (getDataFileName)
import           Data.Aeson (encode)

main :: IO ()
main = do
    db <- newDatabase
    doc <- getDataFileName "resourses/index.html"

    scotty 3000 $ do

        get "/" $
            file doc

        get "/v1/areas.json" $
            jsonUtf8 (areas db)

        get "/v1/documents.json" $
            jsonUtf8 (documents db)

        get "/v1/expenses.json" $
            jsonUtf8 (expenses db)

        where jsonUtf8 xs = do
                setHeader "Content-Type" "application/json; charset=utf-8"
                raw $ encode xs
