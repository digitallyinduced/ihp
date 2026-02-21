{-|
Module: IHP.AutoRefresh.Types
Description: Types & Data Structures for IHP AutoRefresh
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.AutoRefresh.Types where

import Control.Concurrent.MVar (MVar)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.UUID as UUID
import qualified IHP.PGListener as PGListener
import IHP.Prelude
import qualified Data.Map.Strict as Map
import Network.Wai (Request)
import Wai.Request.Params.Middleware (Respond)

-- | A database operation that can trigger an auto refresh re-render.
data AutoRefreshOperation
    = AutoRefreshInsert
    | AutoRefreshUpdate
    | AutoRefreshDelete
    deriving (Eq, Show)

instance Aeson.FromJSON AutoRefreshOperation where
    parseJSON = Aeson.withText "AutoRefreshOperation" \operation ->
        case toLower operation of
            "insert" -> pure AutoRefreshInsert
            "update" -> pure AutoRefreshUpdate
            "delete" -> pure AutoRefreshDelete
            _        -> fail ("Unknown operation: " <> cs operation)

-- | Internal: raw payload sent by the PostgreSQL trigger.
--
-- For oversized payloads the trigger stores the full JSON in @large_pg_notifications@ and sends only a @payloadId@.
-- The auto refresh server resolves these @payloadId@s via a database lookup before building the change notification,
-- so smart filtering receives the full row json in @old@/@new@ (if payload resolution fails, auto refresh falls back
-- to forcing a refresh).
data AutoRefreshRowChangePayload = AutoRefreshRowChangePayload
    { payloadOperation      :: !AutoRefreshOperation
    , payloadOldRow         :: !(Maybe Aeson.Value)
    , payloadNewRow         :: !(Maybe Aeson.Value)
    , payloadLargePayloadId :: !(Maybe UUID.UUID)
    } deriving (Eq, Show)

instance Aeson.FromJSON AutoRefreshRowChangePayload where
    parseJSON = Aeson.withObject "AutoRefreshRowChangePayload" \object ->
        AutoRefreshRowChangePayload
            <$> object Aeson..: "op"
            <*> object Aeson..:? "old"
            <*> object Aeson..:? "new"
            <*> do
                payloadId <- object Aeson..:? "payloadId"
                case payloadId of
                    Nothing    -> pure Nothing
                    Just value -> Just <$> parseUUID value
        where
            parseUUID :: Text -> AesonTypes.Parser UUID.UUID
            parseUUID value = case UUID.fromText value of
                Just uuid -> pure uuid
                Nothing   -> fail "Invalid UUID for payloadId"

data AutoRefreshState = AutoRefreshEnabled { sessionId :: !UUID }

data AutoRefreshSession = AutoRefreshSession
        { id :: !UUID
        -- | A callback to rerun an action within the given request and respond
        , renderView :: !(Request -> Respond -> IO ())
        -- | MVar that is filled whenever some table changed
        , event :: !(MVar ())
        -- | All tables this auto refresh session watches
        , tables :: !(Set Text)
        -- | The last rendered html of this action. Initially this is the result of the initial page rendering
        , lastResponse :: !LByteString
        -- | Keep track of the last ping to this session to close it after too much time has passed without anything happening
        , lastPing :: !UTCTime
        -- | Tracked row IDs per table. 'Nothing' for a table means we can't filter (raw SQL / fetchCount).
        -- 'Just ids' means only these IDs are relevant. Used by smart auto refresh to skip unrelated notifications.
        , trackedIds :: !(Map.Map Text (Set Text))
        }

data AutoRefreshServer = AutoRefreshServer
        { subscriptions :: [PGListener.Subscription]
        , sessions :: ![AutoRefreshSession]
        , subscribedTables :: !(Set Text)
        , pgListener :: PGListener.PGListener
        }

newAutoRefreshServer :: PGListener.PGListener -> AutoRefreshServer
newAutoRefreshServer pgListener = AutoRefreshServer { subscriptions = [], sessions = [], subscribedTables = mempty, pgListener }
