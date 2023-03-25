module IHP.DataSync.REST.Types where

import IHP.Prelude

data ApiController
    = CreateRecordAction { table :: Text } -- ^ POST /api/books
    | UpdateRecordAction { table :: Text, id :: UUID }
    | DeleteRecordAction { table :: Text, id :: UUID }
    | ShowRecordAction   { table :: Text, id :: UUID } -- ^ GET /api/books/9ba0ffbc-bfc1-4d7d-8152-30a6648806f7
    | ListRecordsAction  { table :: Text } -- ^ GET /api/books
    | GraphQLQueryAction
    deriving (Eq, Show, Data)
