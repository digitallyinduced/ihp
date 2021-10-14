module IHP.DataSync.REST.Routes where

import IHP.RouterPrelude
import IHP.DataSync.REST.Types

instance CanRoute ApiController where
    parseRoute' = do
        string "/api/"
        table <- parseText
        let
            createRecordAction = do
                endOfInput
                onlyAllowMethods [POST]
                pure CreateRecordAction { table }

            updateOrDeleteRecordAction = do
                string "/"
                id <- parseUUID

                endOfInput

                method <- getMethod
                case method of
                    PATCH  -> pure UpdateRecordAction { table, id }
                    GET    -> pure ShowRecordAction { table, id }
                    DELETE -> pure DeleteRecordAction { table, id }

        updateOrDeleteRecordAction <|> createRecordAction

instance HasPath ApiController where
    pathTo CreateRecordAction { table } = "/api/" <> table
    pathTo UpdateRecordAction { table, id } = "/api/" <> table <> "/" <> tshow id
    pathTo DeleteRecordAction { table, id } = "/api/" <> table <> "/" <> tshow id
