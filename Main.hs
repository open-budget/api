{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson                (encode)
import           OpenBudget.Builder
import qualified OpenBudget.Types.Area     as Area (select)
import           OpenBudget.Types.Database
import qualified OpenBudget.Types.Document as Document (select)
import qualified OpenBudget.Types.Expense  as Expense (select)
import           Paths_api                 (getDataFileName)
import           Web.Scotty
import qualified Data.Map as Map (Map, fromList)

main :: IO ()
main = do

    -- todo: перенести БД у IORef та оновляти кожного разу,
    --       коли оновлюється директорія `data`
    db <- newDatabase

    doc <- getDataFileName "resourses/index.html"

    scotty 3000 $ do

        get "/" $
            file doc

        get "/areas" $ do
            params' <- params
            jsonUtf8 $ Area.select params' (areas db)

        get "/documents" $ do
            params' <- params
            jsonUtf8 $ Document.select params' (documents db)

        get "/expenses" $ do
            params' <- params
            jsonUtf8 $ Expense.select params' (expenses db)

        get "/links" $
            raw $ encode (Map.fromList [ ("areas",     "https://api.open-budget.org/areas")
                                       , ("documents", "https://api.open-budget.org/areas")
                                       , ("expenses",  "https://api.open-budget.org/expenses")
                                       ] :: Map.Map String String)

        where jsonUtf8 xs = do
                setHeader "Access-Control-Allow-Origin" "*"
                setHeader "Content-Type" "application/json; charset=utf-8"
                raw $ encode xs
