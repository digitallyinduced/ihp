{-# LANGUAGE UndecidableInstances #-}
module IHP.DataSync.Controller where

import IHP.ControllerPrelude hiding (OrderByClause)
import qualified Control.Exception as Exception
import qualified IHP.Log as Log
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding.Internal as Aeson

import Data.Aeson.TH
import Data.Aeson
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.ToField as PG
import qualified Database.PostgreSQL.Simple.Types as PG
import qualified Data.HashMap.Strict as HashMap
import qualified Data.UUID.V4 as UUID
import qualified Control.Concurrent.MVar as MVar
import IHP.DataSync.Types
import IHP.DataSync.RowLevelSecurity
import IHP.DataSync.DynamicQuery
import IHP.DataSync.DynamicQueryCompiler
import qualified IHP.DataSync.ChangeNotifications as ChangeNotifications
import IHP.DataSync.REST.Controller (aesonValueToPostgresValue)
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Builder as ByteString
import qualified IHP.PGListener as PGListener
import IHP.ApplicationContext
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Pool as Pool

import qualified IHP.GraphQL.Types as GraphQL
import qualified IHP.GraphQL.Parser as GraphQL
import qualified IHP.GraphQL.Compiler as GraphQL
import IHP.GraphQL.JSON ()
import qualified Data.Attoparsec.Text as Attoparsec
import qualified Network.WebSockets as WS
import IHP.DataSync.ControllerImpl (runDataSyncController, cleanupAllSubscriptions)

instance (
    PG.ToField (PrimaryKey (GetTableName CurrentUserRecord))
    , Show (PrimaryKey (GetTableName CurrentUserRecord))
    , HasNewSessionUrl CurrentUserRecord
    , Typeable CurrentUserRecord
    , HasField "id" CurrentUserRecord (Id' (GetTableName CurrentUserRecord))
    ) => WSApp DataSyncController where
    initialState = DataSyncController

    run = runDataSyncController
    onClose = cleanupAllSubscriptions
