{-# LANGUAGE UndecidableInstances #-}
module IHP.DataSync.Controller where

import IHP.ControllerPrelude hiding (OrderByClause)

import IHP.DataSync.Types
import IHP.DataSync.RowLevelSecurity
import qualified IHP.DataSync.ChangeNotifications as ChangeNotifications
import IHP.DataSync.ControllerImpl (runDataSyncController)
import IHP.DataSync.DynamicQueryCompiler (camelCaseRenamer)

instance (
    Show (Id' (GetTableName CurrentUserRecord))
    , HasNewSessionUrl CurrentUserRecord
    , Typeable CurrentUserRecord
    , HasField "id" CurrentUserRecord (Id' (GetTableName CurrentUserRecord))
    ) => WSApp DataSyncController where
    initialState = DataSyncController

    run = do
        let hasqlPool = ?modelContext.hasqlPool
        ensureRLSEnabled <- makeCachedEnsureRLSEnabled hasqlPool
        installTableChangeTriggers <- ChangeNotifications.makeInstallTableChangeTriggers ?context.frameworkConfig.environment hasqlPool
        runDataSyncController hasqlPool ensureRLSEnabled installTableChangeTriggers (receiveData @ByteString) sendJSON (\_ _ -> pure ()) (\_ -> camelCaseRenamer)
