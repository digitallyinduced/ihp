{-# LANGUAGE AllowAmbiguousTypes #-}
{-|
Module: IHP.AutoRefresh.Types
Description: Types & Data Structures for IHP AutoRefresh
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.AutoRefresh.Types where

import Control.Concurrent.MVar (MVar)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKeyMap
import qualified Data.Aeson.Types as AesonTypes
import Data.Semigroup (Semigroup (..))
import qualified Data.UUID as UUID
import qualified IHP.PGListener as PGListener
import IHP.Prelude
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

-- | Describes a single row change received by 'IHP.AutoRefresh.autoRefreshWith'.
--
-- For updates and deletes the old and new row json values are available.
data AutoRefreshRowChange = AutoRefreshRowChange
    { table     :: !Text
    -- ^ Table name as used in SQL (e.g. @"projects"@)
    , operation :: !AutoRefreshOperation
    -- ^ Whether this change was caused by an insert, update or delete
    , oldRow    :: !(Maybe Aeson.Value)
    -- ^ Full row json before the change (only present for updates and deletes)
    , newRow    :: !(Maybe Aeson.Value)
    -- ^ Full row json after the change (only present for inserts and updates)
    } deriving (Eq, Show)

-- | A batch of row changes.
--
-- The 'AutoRefreshChangeSet' is passed to 'IHP.AutoRefresh.AutoRefreshOptions.shouldRefresh' and contains all changes
-- accumulated since the last refresh tick.
newtype AutoRefreshChangeSet = AutoRefreshChangeSet
    { changes :: [AutoRefreshRowChange]
    } deriving (Eq, Show)

instance Semigroup AutoRefreshChangeSet where
    AutoRefreshChangeSet left <> AutoRefreshChangeSet right = AutoRefreshChangeSet (left <> right)

instance Monoid AutoRefreshChangeSet where
    mempty = AutoRefreshChangeSet []

-- | Internal: raw payload sent by the PostgreSQL trigger.
--
-- For oversized payloads the trigger stores the full JSON in @large_pg_notifications@ and sends only a @payloadId@.
-- The auto refresh server resolves these @payloadId@s via a database lookup before building the
-- 'AutoRefreshChangeSet', so 'IHP.AutoRefresh.AutoRefreshOptions.shouldRefresh' normally receives the full
-- row json in @old@/@new@ (if payload resolution fails, auto refresh falls back to forcing a refresh).
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

-- | Internal: Inserts a row change into the change set.
insertRowChange :: Text -> AutoRefreshRowChangePayload -> AutoRefreshChangeSet -> AutoRefreshChangeSet
insertRowChange tableName AutoRefreshRowChangePayload { payloadOperation, payloadOldRow, payloadNewRow } (AutoRefreshChangeSet existing) =
    AutoRefreshChangeSet (AutoRefreshRowChange { table = tableName, operation = payloadOperation, oldRow = payloadOldRow, newRow = payloadNewRow } : existing)

-- | Internal: Inserts a row change into the change set, using either the new row or the old row json.
insertRowChangeFromPayload :: Text -> AutoRefreshRowChangePayload -> AutoRefreshChangeSet -> AutoRefreshChangeSet
insertRowChangeFromPayload tableName payload changeSet =
    insertRowChange tableName payload changeSet

-- | Returns all changes related to a given table.
changesForTable :: Text -> AutoRefreshChangeSet -> [AutoRefreshRowChange]
changesForTable tableName = filter (\change -> change.table == tableName) . (.changes)

-- | Returns row json values related to a given table.
--
-- Prefers @newRow@ over @oldRow@ for each change.
rowsForTable :: Text -> AutoRefreshChangeSet -> [Aeson.Value]
rowsForTable tableName changeSet =
    mapMaybe (\change -> change.newRow <|> change.oldRow) (changesForTable tableName changeSet)

-- | Returns @True@ when at least one row change happened on the given table.
anyChangeOnTable :: Text -> AutoRefreshChangeSet -> Bool
anyChangeOnTable tableName = not . null . changesForTable tableName

-- | Checks if any changed row (across all tables) contains the given field with the expected value.
--
-- The field name is treated as a Haskell record field name and converted to snake_case to match SQL column names:
--
-- > anyChangeWithField @"userId" userId changes
anyChangeWithField :: forall field value. (KnownSymbol field, Aeson.FromJSON value, Eq value) => value -> AutoRefreshChangeSet -> Bool
anyChangeWithField value (AutoRefreshChangeSet existing) =
    any (\change -> rowField @field change == Just value) existing

-- | Reads a field from the changed row data.
--
-- Prefers @newRow@ and falls back to @oldRow@.
rowField :: forall field value. (KnownSymbol field, Aeson.FromJSON value) => AutoRefreshRowChange -> Maybe value
rowField change = rowFieldNew @field change <|> rowFieldOld @field change

-- | Reads a field from the new row data (after the update).
rowFieldNew :: forall field value. (KnownSymbol field, Aeson.FromJSON value) => AutoRefreshRowChange -> Maybe value
rowFieldNew change = do
    row <- change.newRow
    rowFieldByColumnName (fieldNameToColumnName (symbolToText @field)) row

-- | Reads a field from the old row data (before the update).
rowFieldOld :: forall field value. (KnownSymbol field, Aeson.FromJSON value) => AutoRefreshRowChange -> Maybe value
rowFieldOld change = do
    row <- change.oldRow
    rowFieldByColumnName (fieldNameToColumnName (symbolToText @field)) row

-- | Reads a field from a row json object by SQL column name.
rowFieldByColumnName :: Aeson.FromJSON value => Text -> Aeson.Value -> Maybe value
rowFieldByColumnName columnName = \case
    Aeson.Object object -> do
        value <- AesonKeyMap.lookup (AesonKey.fromText columnName) object
        AesonTypes.parseMaybe Aeson.parseJSON value
    _ -> Nothing

data AutoRefreshState = AutoRefreshDisabled | AutoRefreshEnabled { sessionId :: !UUID }

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
        }
    | AutoRefreshSessionWithChanges
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
        -- | Pending changes accumulated since the last refresh tick
        --
        -- When set to 'Nothing' the next refresh tick will always re-render (used as a fallback when a change payload
        -- could not be resolved from @large_pg_notifications@).
        , pendingChanges :: !(IORef (Maybe AutoRefreshChangeSet))
        -- | Decide if a refresh should run for the accumulated changes
        , shouldRefresh :: !(AutoRefreshChangeSet -> IO Bool)
        }

data AutoRefreshServer = AutoRefreshServer
        { subscriptions :: [PGListener.Subscription]
        , sessions :: ![AutoRefreshSession]
        , subscribedTables :: !(Set Text)
        , subscribedRowTables :: !(Set Text)
        , pgListener :: PGListener.PGListener
        }

newAutoRefreshServer :: PGListener.PGListener -> AutoRefreshServer
newAutoRefreshServer pgListener = AutoRefreshServer { subscriptions = [], sessions = [], subscribedTables = mempty, subscribedRowTables = mempty, pgListener }
