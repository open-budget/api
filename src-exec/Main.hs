{-# LANGUAGE OverloadedStrings #-}

module Main where

import           OpenBudget.Builder
import           OpenBudget.Types.Database
import           OpenBudget.Types.Document hiding (fromCSV)
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

        get "/v1/documents.json" $ do
            params' <- params
            jsonUtf8 $ select params' (documents db)

        get "/v1/expenses.json" $
            jsonUtf8 (expenses db)

        where jsonUtf8 xs = do
                setHeader "Access-Control-Allow-Origin" "*"
                setHeader "Content-Type" "application/json; charset=utf-8"
                raw $ encode xs
