{-# LANGUAGE UndecidableInstances #-}
module IHP.DataSync.Controller where

import IHP.ControllerPrelude hiding (OrderByClause)

import IHP.DataSync.Types
import IHP.DataSync.RowLevelSecurity
import qualified IHP.DataSync.ChangeNotifications as ChangeNotifications
import IHP.DataSync.ControllerImpl (runDataSyncController)
import IHP.DataSync.DynamicQueryCompiler (camelCaseRenamer)
import qualified Hasql.Pool
import qualified Hasql.Pool.Config as Hasql.Pool.Config
import qualified Hasql.Connection.Setting as HasqlSetting
import qualified Hasql.Connection.Setting.Connection as HasqlConnection
import qualified Control.Exception.Safe as Exception

instance (
    Show (PrimaryKey (GetTableName CurrentUserRecord))
    , HasNewSessionUrl CurrentUserRecord
    , Typeable CurrentUserRecord
    , HasField "id" CurrentUserRecord (Id' (GetTableName CurrentUserRecord))
    ) => WSApp DataSyncController where
    initialState = DataSyncController

    run = do
        let hasqlPoolConfig = Hasql.Pool.Config.settings
                [ Hasql.Pool.Config.size 10
                , Hasql.Pool.Config.staticConnectionSettings [HasqlSetting.connection (HasqlConnection.string (cs ?modelContext.databaseUrl))]
                ]
        Exception.bracket (Hasql.Pool.acquire hasqlPoolConfig) Hasql.Pool.release \hasqlPool -> do
            ensureRLSEnabled <- makeCachedEnsureRLSEnabled hasqlPool
            installTableChangeTriggers <- ChangeNotifications.makeCachedInstallTableChangeTriggers hasqlPool
            runDataSyncController hasqlPool ensureRLSEnabled installTableChangeTriggers (receiveData @ByteString) sendJSON (\_ _ -> pure ()) (\_ -> camelCaseRenamer)