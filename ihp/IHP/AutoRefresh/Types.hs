{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-|
Module: IHP.AutoRefresh.Types
Description: Types & Data Structures for IHP AutoRefresh
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.AutoRefresh.Types where

import           Control.Concurrent.MVar       (MVar)
import qualified Data.Aeson                    as Aeson
import qualified Data.Aeson.KeyMap             as AesonKeyMap
import qualified Data.Aeson.Types              as AesonTypes
import           Data.Semigroup                (Semigroup (..))
import           Data.Set                      (Set)
import qualified Data.UUID                     as UUID
import qualified IHP.PGListener                as PGListener
import           IHP.Prelude
import           Network.Wai                   (Request)
import           Wai.Request.Params.Middleware (Respond)

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

data AutoRefreshRowChange = AutoRefreshRowChange
    { table     :: !ByteString
    , operation :: !AutoRefreshOperation
    , row       :: !Aeson.Value
    , oldRow    :: !(Maybe Aeson.Value)
    , newRow    :: !(Maybe Aeson.Value)
    } deriving (Eq, Show)

newtype AutoRefreshChangeSet = AutoRefreshChangeSet
    { changes :: [AutoRefreshRowChange]
    } deriving (Eq, Show)

instance Semigroup AutoRefreshChangeSet where
    AutoRefreshChangeSet left <> AutoRefreshChangeSet right = AutoRefreshChangeSet (left <> right)

instance Monoid AutoRefreshChangeSet where
    mempty = AutoRefreshChangeSet []

data AutoRefreshRowChangePayload = AutoRefreshRowChangePayload
    { payloadOperation      :: !AutoRefreshOperation
    , payloadRowId          :: !Aeson.Value
    , payloadOldRow         :: !(Maybe Aeson.Value)
    , payloadNewRow         :: !(Maybe Aeson.Value)
    , payloadLargePayloadId :: !(Maybe UUID.UUID)
    } deriving (Eq, Show)

instance Aeson.FromJSON AutoRefreshRowChangePayload where
    parseJSON = Aeson.withObject "AutoRefreshRowChangePayload" \object ->
        AutoRefreshRowChangePayload
            <$> object Aeson..: "op"
            <*> object Aeson..: "id"
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

insertRowChange :: ByteString -> AutoRefreshRowChangePayload -> Aeson.Value -> AutoRefreshChangeSet -> AutoRefreshChangeSet
insertRowChange tableName AutoRefreshRowChangePayload { payloadOperation, payloadOldRow, payloadNewRow } row (AutoRefreshChangeSet existing) =
    AutoRefreshChangeSet (AutoRefreshRowChange { table = tableName, operation = payloadOperation, row, oldRow = payloadOldRow, newRow = payloadNewRow } : existing)

insertRowChangeFromPayload :: ByteString -> AutoRefreshRowChangePayload -> AutoRefreshChangeSet -> AutoRefreshChangeSet
insertRowChangeFromPayload tableName payload changeSet =
    insertRowChange tableName payload rowValue changeSet
    where
        -- When the payload omits full row data (e.g. large payloads), we still keep the
        -- row id so change coalescing can key off the primary key.
        rowValue = fromMaybe (payloadRowId payload) (payloadNewRow payload <|> payloadOldRow payload)

changesForTable :: ByteString -> AutoRefreshChangeSet -> [AutoRefreshRowChange]
changesForTable tableName = filter (\change -> change.table == tableName) . (.changes)

rowsForTable :: ByteString -> AutoRefreshChangeSet -> [Aeson.Value]
rowsForTable tableName = map (.row) . changesForTable tableName

anyChangeOnTable :: ByteString -> AutoRefreshChangeSet -> Bool
anyChangeOnTable tableName = not . null . changesForTable tableName

anyChangeWithField :: forall field value. (KnownSymbol field, Aeson.FromJSON value, Eq value) => value -> AutoRefreshChangeSet -> Bool
anyChangeWithField value (AutoRefreshChangeSet existing) =
    any (\change -> rowField @field change == Just value) existing

rowField :: forall field value. (KnownSymbol field, Aeson.FromJSON value) => AutoRefreshRowChange -> Maybe value
rowField change = rowFieldByColumnName (fieldNameToColumnName (symbolToText @field)) rowValue
    where
        rowValue = fromMaybe change.row (change.newRow <|> change.oldRow)

rowFieldNew :: forall field value. (KnownSymbol field, Aeson.FromJSON value) => AutoRefreshRowChange -> Maybe value
rowFieldNew change = change.newRow >>= rowFieldByColumnName (fieldNameToColumnName (symbolToText @field))

rowFieldOld :: forall field value. (KnownSymbol field, Aeson.FromJSON value) => AutoRefreshRowChange -> Maybe value
rowFieldOld change = change.oldRow >>= rowFieldByColumnName (fieldNameToColumnName (symbolToText @field))

rowFieldByColumnName :: forall value. (Aeson.FromJSON value) => Text -> Aeson.Value -> Maybe value
rowFieldByColumnName columnName = \case
    Aeson.Object object -> do
        value <- AesonKeyMap.lookup (cs columnName) object
        AesonTypes.parseMaybe Aeson.parseJSON value
    _ -> Nothing

data AutoRefreshState = AutoRefreshDisabled | AutoRefreshEnabled { sessionId :: !UUID }

data AutoRefreshSession = AutoRefreshSession
        { id           :: !UUID
        -- | A callback to rerun an action within the given request and respond
        , renderView   :: !(Request -> Respond -> IO ())
        -- | MVar that is filled whenever some table changed
        , event        :: !(MVar ())
        -- | All tables this auto refresh session watches
        , tables       :: !(Set ByteString)
        -- | The last rendered html of this action. Initially this is the result of the initial page rendering
        , lastResponse :: !LByteString
        -- | Keep track of the last ping to this session to close it after too much time has passed without anything happening
        , lastPing     :: !UTCTime
        }
    | AutoRefreshSessionWithChanges
        { id             :: !UUID
        -- | A callback to rerun an action within the given request and respond
        , renderView     :: !(Request -> Respond -> IO ())
        -- | MVar that is filled whenever some table changed
        , event          :: !(MVar ())
        -- | All tables this auto refresh session watches
        , tables         :: !(Set ByteString)
        -- | The last rendered html of this action. Initially this is the result of the initial page rendering
        , lastResponse   :: !LByteString
        -- | Keep track of the last ping to this session to close it after too much time has passed without anything happening
        , lastPing       :: !UTCTime
        -- | Pending changes coalesced since the last refresh
        , pendingChanges :: !(IORef AutoRefreshChangeSet)
        -- | Decide if a refresh should run for the accumulated changes
        , shouldRefresh  :: !(AutoRefreshChangeSet -> IO Bool)
        }

data AutoRefreshServer = AutoRefreshServer
        { subscriptions       :: [PGListener.Subscription]
        , sessions            :: ![AutoRefreshSession]
        , subscribedTables    :: !(Set ByteString)
        , subscribedRowTables :: !(Set ByteString)
        , pgListener          :: PGListener.PGListener
        }

newAutoRefreshServer :: PGListener.PGListener -> AutoRefreshServer
newAutoRefreshServer pgListener = AutoRefreshServer { subscriptions = [], sessions = [], subscribedTables = mempty, subscribedRowTables = mempty, pgListener }

