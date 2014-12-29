{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson                 (encode)
import           OpenBudget.Builder
import qualified OpenBudget.Types.Area      as Area (select)
import           OpenBudget.Types.Database
import qualified OpenBudget.Types.Document  as Document (select)
import qualified OpenBudget.Types.Expense   as Expense (select)
import           Paths_api                  (getDataFileName)
import           Web.Scotty

main :: IO ()
main = do
    db <- newDatabase
    doc <- getDataFileName "resourses/index.html"

    scotty 3000 $ do

        get "/" $
            file doc

        get "/v1/areas.json" $ do
            params' <- params
            jsonUtf8 $ Area.select params' (areas db)

        get "/v1/documents.json" $ do
            params' <- params
            jsonUtf8 $ Document.select params' (documents db)

        get "/v1/expenses.json" $ do
            params' <- params
            jsonUtf8 $ Expense.select params' (expenses db)

        where jsonUtf8 xs = do
                setHeader "Access-Control-Allow-Origin" "*"
                setHeader "Content-Type" "application/json; charset=utf-8"
                raw $ encode xs
