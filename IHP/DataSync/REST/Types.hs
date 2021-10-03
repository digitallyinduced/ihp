module IHP.DataSync.REST.Types where

import IHP.Prelude

data ApiController
    = CreateRecordAction { table :: Text } -- ^ POST /api/books
    | UpdateRecordAction { table :: Text, id :: UUID }
    | DeleteRecordAction { table :: Text, id :: UUID }
    deriving (Eq, Show, Data)
