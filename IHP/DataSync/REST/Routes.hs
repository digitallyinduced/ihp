module IHP.DataSync.REST.Routes where

import IHP.RouterPrelude
import IHP.DataSync.REST.Types

instance CanRoute ApiController where
    parseRoute' = do
        string "/api/"

        let
            graphQLQueryAction = do
                string "graphql"
                endOfInput
                onlyAllowMethods [POST]
                pure GraphQLQueryAction

            createRecordAction table = do
                endOfInput
                onlyAllowMethods [POST]
                pure CreateRecordAction { table }

            updateOrDeleteRecordAction table = do
                string "/"
                id <- parseUUID

                endOfInput

                method <- getMethod
                case method of
                    PATCH  -> pure UpdateRecordAction { table, id }
                    GET    -> pure ShowRecordAction { table, id }
                    DELETE -> pure DeleteRecordAction { table, id }

            listRecordsAction table = do
                endOfInput
                method <- getMethod
                case method of
                    GET -> pure ListRecordsAction { table }

            crud = do
                table <- parseText
                updateOrDeleteRecordAction table <|> createRecordAction table <|> listRecordsAction table

        graphQLQueryAction <|> crud

instance HasPath ApiController where
    pathTo CreateRecordAction { table } = "/api/" <> table
    pathTo UpdateRecordAction { table, id } = "/api/" <> table <> "/" <> tshow id
    pathTo DeleteRecordAction { table, id } = "/api/" <> table <> "/" <> tshow id
    pathTo GraphQLQueryAction = "/api/graphql"