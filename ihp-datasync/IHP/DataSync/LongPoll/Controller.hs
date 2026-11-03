{-# LANGUAGE UndecidableInstances #-}
{-|
Module: IHP.DataSync.LongPoll.Controller
Description: HTTP endpoints of the DataSync long polling transport

Mount the controller next to the DataSync WebSocket:

> instance FrontController WebApplication where
>     controllers =
>         [ webSocketApp @DataSyncController
>         , parseRoute @DataSyncLongPollController
>         ]

The JavaScript client switches to these endpoints when it cannot open the
WebSocket. Every request of a connection must reach the same server process.
-}
module IHP.DataSync.LongPoll.Controller where

import IHP.ControllerPrelude hiding (OrderByClause)
import IHP.DataSync.LongPoll.Types
import IHP.DataSync.LongPoll
import IHP.DataSync.Types (DataSyncController (..))
import IHP.DataSync.ControllerImpl (runDataSyncController)
import IHP.DataSync.RowLevelSecurity (makeCachedEnsureRLSEnabled)
import IHP.DataSync.DynamicQueryCompiler (camelCaseRenamer)
import qualified IHP.DataSync.ChangeNotifications as ChangeNotifications
import qualified Data.Aeson as Aeson
import qualified Data.Vector as Vector
import Network.HTTP.Types (status200, status400, status404)
import Network.HTTP.Types.Header (hCacheControl, hContentType)
import Network.Wai (responseLBS)

instance (
    Show (PrimaryKey (GetTableName CurrentUserRecord))
    , HasNewSessionUrl CurrentUserRecord
    , Typeable CurrentUserRecord
    , HasField "id" CurrentUserRecord (Id' (GetTableName CurrentUserRecord))
    ) => Controller DataSyncLongPollController where
    action OpenLongPollAction = do
        let hasqlPool = ?modelContext.hasqlPool
        ensureRLSEnabled <- makeCachedEnsureRLSEnabled hasqlPool
        installTableChangeTriggers <- ChangeNotifications.makeInstallTableChangeTriggers ?request.frameworkConfig.environment hasqlPool
        dataSyncState <- newIORef DataSyncController
        connection <- openLongPollConnection globalLongPollRegistry defaultLongPollConfig longPollOwnerId \receiveData sendMessage -> do
            let ?state = dataSyncState
            runDataSyncController hasqlPool ensureRLSEnabled installTableChangeTriggers receiveData (sendMessage . Aeson.encode) (\_ _ -> pure ()) (\_ -> camelCaseRenamer)
        renderJson (Aeson.object ["connectionId" .= connection.connectionId])

    action SendLongPollAction { connectionId } = withLongPollConnection connectionId \connection -> do
        payload <- requestBodyJSON
        case payload of
            Aeson.Array messages -> do
                pushClientMessages connection (map (cs . Aeson.encode) (Vector.toList messages))
                renderJson (Aeson.object [])
            _ -> renderJsonWithStatusCode status400 (Aeson.object ["error" .= ("Expected a JSON array of DataSync messages" :: Text)])

    action ReceiveLongPollAction { connectionId } = withLongPollConnection connectionId \connection -> do
        let acknowledged = paramOrDefault @Int 0 "after"
        batch <- awaitServerMessages defaultLongPollConfig connection acknowledged
        respondWith $ responseLBS status200
            [ (hContentType, "application/json")
            , (hCacheControl, "no-store")
            ]
            (encodeLongPollBatch batch)

    action CloseLongPollAction { connectionId } = withLongPollConnection connectionId \connection -> do
        closeLongPollConnection connection
        renderJson (Aeson.object [])

-- | Runs the handler with the connection, or responds with 404 when the
-- connection is closed or belongs to another user
withLongPollConnection ::
    ( ?request :: Request
    , ?respond :: Respond
    , Typeable CurrentUserRecord
    , HasField "id" CurrentUserRecord (Id' (GetTableName CurrentUserRecord))
    , Show (PrimaryKey (GetTableName CurrentUserRecord))
    ) => UUID -> (LongPollConnection -> IO ResponseReceived) -> IO ResponseReceived
withLongPollConnection connectionId handle = do
    found <- lookupLongPollConnection globalLongPollRegistry connectionId longPollOwnerId
    case found of
        Just connection -> handle connection
        Nothing -> renderJsonWithStatusCode status404 (Aeson.object ["error" .= ("Unknown DataSync connection" :: Text)])

longPollOwnerId ::
    ( ?request :: Request
    , Typeable CurrentUserRecord
    , HasField "id" CurrentUserRecord (Id' (GetTableName CurrentUserRecord))
    , Show (PrimaryKey (GetTableName CurrentUserRecord))
    ) => Maybe Text
longPollOwnerId = tshow . (.id) <$> currentUserOrNothing @CurrentUserRecord
