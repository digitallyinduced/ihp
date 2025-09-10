{-# LANGUAGE UndecidableInstances #-}
module IHP.DataSync.Controller where

import IHP.ControllerPrelude hiding (OrderByClause)

import IHP.DataSync.Types
import IHP.DataSync.RowLevelSecurity
import qualified Database.PostgreSQL.Simple.ToField as PG
import qualified IHP.DataSync.ChangeNotifications as ChangeNotifications
import IHP.DataSync.ControllerImpl (runDataSyncController)
import IHP.DataSync.DynamicQueryCompiler (camelCaseRenamer)

instance (
    PG.ToField (PrimaryKey (GetTableName CurrentUserRecord))
    , Show (PrimaryKey (GetTableName CurrentUserRecord))
    , HasNewSessionUrl CurrentUserRecord
    , Typeable CurrentUserRecord
    , HasField "id" CurrentUserRecord (Id' (GetTableName CurrentUserRecord))
    ) => WSApp DataSyncController where
    initialState = DataSyncController

    run = do
        ensureRLSEnabled <- makeCachedEnsureRLSEnabled
        installTableChangeTriggers <- ChangeNotifications.makeCachedInstallTableChangeTriggers
        runDataSyncController ensureRLSEnabled installTableChangeTriggers (receiveData @ByteString) sendJSON (\_ _ -> pure ()) (\_ -> camelCaseRenamer)