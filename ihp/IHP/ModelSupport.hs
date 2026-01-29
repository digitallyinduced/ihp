{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts, AllowAmbiguousTypes, UndecidableInstances, FlexibleInstances, IncoherentInstances, DataKinds, PolyKinds, TypeApplications, ScopedTypeVariables, ConstraintKinds, TypeOperators, GADTs, GeneralizedNewtypeDeriving #-}

module IHP.ModelSupport
( module IHP.ModelSupport
, module IHP.ModelSupport.Types
, module IHP.Postgres.Point
, module IHP.Postgres.Polygon
, module IHP.Postgres.Inet
, module IHP.Postgres.TSVector
, module IHP.Postgres.TimeParser
, module IHP.InputValue
) where

import IHP.ModelSupport.Types

import IHP.HaskellSupport
import IHP.NameSupport
import IHP.InputValue
import Prelude
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Int (Int16, Int32, Int64)
import Data.IORef (IORef, newIORef, modifyIORef')
import Data.Hashable (Hashable)
import Control.DeepSeq (NFData)
import Control.Exception (finally, throwIO, catch, Exception, SomeException)
import Data.Maybe (fromMaybe, isNothing)
import Data.List (filter, elem)
import qualified Data.ByteString.Char8 as BS8
import Data.String (IsString(..))
import Data.Default
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.String.Conversions (cs ,ConvertibleStrings)
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Calendar
import Data.UUID hiding (fromString)
import GHC.Records
import GHC.TypeLits
import GHC.Types
import Data.Proxy
import Data.Data
import Data.Aeson (ToJSON (..), FromJSON (..))
import qualified Data.Aeson as Aeson
import qualified Data.Set as Set
import qualified Text.Read as Read
import IHP.Postgres.Point
import IHP.Postgres.Interval hiding (pPGInterval)
import IHP.Postgres.Polygon
import IHP.Postgres.Inet
import IHP.Postgres.TSVector
import IHP.Postgres.TimeParser
import qualified Net.IP as IP
import Net.IP (IP)
import Data.Functor.Contravariant (contramap)
import Data.Attoparsec.ByteString.Char8 (parseOnly)
import IHP.Log.Types
import qualified IHP.Log as Log
import Data.Dynamic
import Data.Scientific
import GHC.Stack
import qualified Numeric
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString.Builder as Builder

-- hasql imports
import qualified Hasql.Connection as Hasql
import qualified Hasql.Connection.Setting as HasqlSetting
import qualified Hasql.Connection.Setting.Connection as HasqlConnection
import qualified Hasql.Session as Hasql
import qualified Hasql.Statement as Hasql
import qualified Hasql.Pool as HasqlPool
import qualified Hasql.Pool.Config as HasqlPoolConfig
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.DynamicStatements.Snippet as Snippet
import Hasql.DynamicStatements.Snippet (Snippet)
import qualified Hasql.DynamicStatements.Session as DynSession
import Hasql.Implicits.Encoders (DefaultParamEncoder(..))



-- | Provides a mock ModelContext to be used when a database connection is not available
notConnectedModelContext :: Logger -> ModelContext
notConnectedModelContext logger = ModelContext
    { connectionPool = error "Not connected"
    , transactionConnection = Nothing
    , logger = logger
    , trackTableReadCallback = Nothing
    , rowLevelSecurity = Nothing
    , databaseUrl = error "Not connected"
    }

createModelContext :: NominalDiffTime -> Int -> ByteString -> Logger -> IO ModelContext
createModelContext idleTime maxConnections databaseUrl logger = do
    let poolSettings =
            [ HasqlPoolConfig.size maxConnections
            , HasqlPoolConfig.idlenessTimeout (realToFrac idleTime)
            , HasqlPoolConfig.staticConnectionSettings (connectionSettingsFromDatabaseUrl databaseUrl)
            ]
    connectionPool <- HasqlPool.acquire (HasqlPoolConfig.settings poolSettings)

    let trackTableReadCallback = Nothing
    let transactionConnection = Nothing
    let rowLevelSecurity = Nothing
    pure ModelContext { .. }

releaseModelContext :: ModelContext -> IO ()
releaseModelContext modelContext =
    HasqlPool.release modelContext.connectionPool

{-# INLINE createRecord #-}
createRecord :: (?modelContext :: ModelContext, CanCreate model) => model -> IO model
createRecord = create

instance Default Text where
    {-# INLINE def #-}
    def = ""

instance Default Point where
    def = Point def def

instance Default Polygon where
    def = Polygon [def]

instance Default TSVector where
    def = TSVector def

instance Default Scientific where
    def = 0

-- | Returns @True@ when the record has not been saved to the database yet. Returns @False@ otherwise.
--
-- __Example:__ Returns @True@ when a record has not been inserted yet.
--
-- >>> let project = newRecord @Project
-- >>> isNew project
-- True
--
-- __Example:__ Returns @False@ after inserting a record.
--
-- >>> project <- createRecord project
-- >>> isNew project
-- False
--
-- __Example:__ Returns @False@ for records which have been fetched from the database.
--
-- >>> book <- query @Book |> fetchOne
-- >>> isNew book
-- False
isNew :: forall model. (HasField "meta" model MetaBag) => model -> Bool
isNew model = isNothing model.meta.originalDatabaseRecord
{-# INLINABLE isNew #-}

-- | Returns the model name of a given model as Text
--
-- __Example:__
--
-- >>> modelName @User
-- "User"
--
-- >>> modelName @Project
-- "Project"
getModelName :: forall model. KnownSymbol (GetModelName model) => Text
getModelName = cs $! symbolVal (Proxy :: Proxy (GetModelName model))
{-# INLINE getModelName #-}

instance InputValue (PrimaryKey model') => InputValue (Id' model') where
    {-# INLINE inputValue #-}
    inputValue = inputValue . unpackId

instance IsEmpty (PrimaryKey table) => IsEmpty (Id' table) where
    isEmpty (Id primaryKey) = isEmpty primaryKey

recordToInputValue :: (HasField "id" entity (Id entity), Show (PrimaryKey (GetTableName entity))) => entity -> Text
recordToInputValue entity =
    entity.id
    |> unpackId
    |> Text.pack . show
{-# INLINE recordToInputValue #-}

instance Show (PrimaryKey model) => Show (Id' model) where
    {-# INLINE show #-}
    show = show . unpackId

-- | Turns an @UUID@ into a @Id@ type
--
-- > let uuid :: UUID = "5240e79c-97ff-4a5f-8567-84112541aaba"
-- > let userId :: Id User = packId uuid
--
packId :: PrimaryKey model -> Id' model
packId uuid = Id uuid

-- | Unwraps a @Id@ value into an @UUID@
--
-- >>> unpackId ("296e5a50-b237-4ee9-83b0-17fb1e6f208f" :: Id User)
-- "296e5a50-b237-4ee9-83b0-17fb1e6f208f" :: UUID
--
unpackId :: Id' model -> PrimaryKey model
unpackId (Id uuid) = uuid

-- | Sometimes you have a hardcoded UUID value which represents some record id. This instance allows you
-- to write the Id like a string:
--
-- > let projectId = "ca63aace-af4b-4e6c-bcfa-76ca061dbdc6" :: Id Project
instance (Read (PrimaryKey model), ParsePrimaryKey (PrimaryKey model)) => IsString (Id' model) where
    fromString uuid = textToId uuid
    {-# INLINE fromString #-}

instance ParsePrimaryKey UUID where
    parsePrimaryKey = Read.readMaybe . cs

instance ParsePrimaryKey Text where
    parsePrimaryKey text = Just text

-- | Transforms a text, bytestring or string into an Id. Throws an exception if the input is invalid.
--
-- __Example:__
--
-- > let projectIdText = "7cbc76e2-1c4f-49b6-a7d9-5015e7575a9b" :: Text
-- > let projectId = (textToId projectIdText) :: Id Project
--
-- In case your UUID value is hardcoded, there is also an 'IsString' instance, so you
-- can just write it like:
--
-- > let projectId = "ca63aace-af4b-4e6c-bcfa-76ca061dbdc6" :: Id Project
textToId :: (HasCallStack, ParsePrimaryKey (PrimaryKey model), ConvertibleStrings text Text) => text -> Id' model
textToId text = case parsePrimaryKey (cs text) of
        Just id -> Id id
        Nothing -> error (cs $ "Unable to convert " <> (cs text :: Text) <> " to Id value. Is it a valid uuid?")
{-# INLINE textToId #-}

-- | Measure and log the query time for a given query action if the log level is Debug.
-- If the log level is greater than debug, just perform the query action without measuring time.
measureTimeIfLogging :: (?modelContext :: ModelContext) => Text -> IO a -> Text -> IO a
measureTimeIfLogging logPrefix queryAction queryText = do
    let currentLogLevel = ?modelContext.logger.level
    if currentLogLevel == Debug
        then do
            start <- getCurrentTime
            queryAction `finally` do
                end <- getCurrentTime
                let theTime = end `diffUTCTime` start
                logQuery logPrefix queryText theTime
        else queryAction

-- | Run a hasql Snippet as a query returning rows.
--
-- This is the primary query execution function for IHP with hasql.
-- It builds a dynamic statement from a Snippet and executes it.
--
-- __Example:__
--
-- > users <- sqlQuery "SELECT id, firstname, lastname FROM users" ()
--
-- Take a look at "IHP.QueryBuilder" for a typesafe approach on building simple queries.
--
-- *AutoRefresh:* When using 'sqlQuery' with AutoRefresh, you need to use 'trackTableRead' to let AutoRefresh know that you have accessed a certain table. Otherwise AutoRefresh will not watch table of your custom sql query.
--
-- Use 'sqlQuerySingleRow' if you expect only a single row to be returned.
--
sqlQuery :: (?modelContext :: ModelContext) => Snippet -> Decoders.Result [a] -> IO [a]
sqlQuery snippet decoder = do
    let snippetWithRLS = withRLSSnippet snippet
    let session = snippetToSession snippetWithRLS decoder
    result <- measureTimeIfLogging "🔍" (runSession session) "<dynamic query>"
    pure result
{-# INLINABLE sqlQuery #-}


-- | Runs a raw sql query, that is expected to return a single result row
--
-- Like 'sqlQuery', but useful when you expect only a single row as the result
--
sqlQuerySingleRow :: (?modelContext :: ModelContext) => Snippet -> Decoders.Result [a] -> IO a
sqlQuerySingleRow snippet decoder = do
    result <- sqlQuery snippet decoder
    case result of
        [] -> error "sqlQuerySingleRow: Expected a single row to be returned."
        [record] -> pure record
        otherwise -> error ("sqlQuerySingleRow: Expected a single row to be returned. But got " <> show (length otherwise) <> " rows")
{-# INLINABLE sqlQuerySingleRow #-}

-- | Runs a sql statement (like a CREATE statement)
--
-- __Example:__
--
-- > sqlExec "CREATE TABLE users ()" ()
sqlExec :: (?modelContext :: ModelContext) => Snippet -> IO ()
sqlExec snippet = do
    let snippetWithRLS = withRLSSnippet snippet
    let session = snippetToSession snippetWithRLS Decoders.noResult
    measureTimeIfLogging "💾" (runSession session) "<dynamic exec>"
{-# INLINABLE sqlExec #-}

-- | Runs a sql statement (like a CREATE statement), but doesn't return any result
--
-- __Example:__
--
-- > sqlExecDiscardResult "CREATE TABLE users ()" ()
sqlExecDiscardResult :: (?modelContext :: ModelContext) => Snippet -> IO ()
sqlExecDiscardResult = sqlExec
{-# INLINABLE sqlExecDiscardResult #-}

-- | Convert a Snippet to a hasql Session for execution
--
-- This uses hasql-dynamic-statements to construct a prepared session from a Snippet and decoder.
-- Prepared statements cache the query plan on the PostgreSQL server, avoiding re-planning
-- for repeated query patterns (only the parameter values change between invocations).
snippetToSession :: Snippet -> Decoders.Result result -> Hasql.Session result
snippetToSession snippet decoder = DynSession.dynamicallyParameterizedStatement snippet decoder True
{-# INLINABLE snippetToSession #-}

-- | Run a hasql session, using the pool or transaction connection
runSession :: (?modelContext :: ModelContext) => Hasql.Session a -> IO a
runSession session =
    case ?modelContext.transactionConnection of
        Just conn -> do
            result <- Hasql.run session conn
            case result of
                Left err -> throwIO (HasqlSessionError err)
                Right val -> pure val
        Nothing -> do
            result <- HasqlPool.use ?modelContext.connectionPool session
            case result of
                Left err -> throwIO (HasqlPoolError err)
                Right val -> pure val
{-# INLINABLE runSession #-}

-- | Wraps the snippet with Row level security boilerplate, if a row level security context was provided
withRLSSnippet :: (?modelContext :: ModelContext) => Snippet -> Snippet
withRLSSnippet snippet = do
    case ?modelContext.rowLevelSecurity of
        Just RowLevelSecurityContext { rlsAuthenticatedRole, rlsUserId } ->
            "SET LOCAL ROLE " <> Snippet.param rlsAuthenticatedRole <> "; SET LOCAL rls.ihp_user_id = " <> rlsUserId <> "; " <> snippet
        Nothing -> snippet

withDatabaseConnection :: (?modelContext :: ModelContext) => (Hasql.Connection -> IO a) -> IO a
withDatabaseConnection block =
    case ?modelContext.transactionConnection of
        Just conn -> block conn
        Nothing -> do
            error "withDatabaseConnection: Not supported with hasql pool. Use runSession instead."
{-# INLINABLE withDatabaseConnection #-}

-- | Runs a raw sql query which results in a single scalar value such as an integer or string
--
-- __Example:__
--
-- > usersCount <- sqlQueryScalar "SELECT COUNT(*) FROM users"
--
-- Take a look at "IHP.QueryBuilder" for a typesafe approach on building simple queries.
sqlQueryScalar :: (?modelContext :: ModelContext) => Snippet -> Decoders.Result a -> IO a
sqlQueryScalar snippet decoder = do
    let snippetWithRLS = withRLSSnippet snippet
    let session = snippetToSession snippetWithRLS decoder
    measureTimeIfLogging "🔍" (runSession session) "<dynamic scalar query>"
{-# INLINABLE sqlQueryScalar #-}

-- | Runs a raw sql query which results in a single scalar value such as an integer or string, or nothing
sqlQueryScalarOrNothing :: (?modelContext :: ModelContext) => Snippet -> Decoders.Result (Maybe a) -> IO (Maybe a)
sqlQueryScalarOrNothing snippet decoder = do
    let snippetWithRLS = withRLSSnippet snippet
    let session = snippetToSession snippetWithRLS decoder
    measureTimeIfLogging "🔍" (runSession session) "<dynamic scalar query>"
{-# INLINABLE sqlQueryScalarOrNothing #-}

-- | Executes the given block with a database transaction
--
-- __Example:__
--
-- > withTransaction do
-- >    company <- newRecord @Company |> createRecord
-- >
-- >    -- When creating the user fails, there will be no company left over
-- >    user <- newRecord @User
-- >        |> set #companyId company.id
-- >        |> createRecord
-- >
-- >    company <- company
-- >        |> set #ownerId user.id
-- >        |> updateRecord
withTransaction :: (?modelContext :: ModelContext) => ((?modelContext :: ModelContext) => IO a) -> IO a
withTransaction block = do
    -- Acquire a dedicated connection for the transaction
    conn <- acquireConnection ?modelContext
    let modelContext = ?modelContext { transactionConnection = Just conn }
    let ?modelContext = modelContext
    let beginSession = Hasql.sql "BEGIN"
    let commitSession = Hasql.sql "COMMIT"
    let rollbackSession = Hasql.sql "ROLLBACK"
    result <- do
        runResult <- Hasql.run beginSession conn
        case runResult of
            Left err -> do
                Hasql.release conn
                throwIO (HasqlSessionError err)
            Right () -> do
                val <- block `catch` (\(e :: SomeException) -> do
                    _ <- Hasql.run rollbackSession conn
                    Hasql.release conn
                    throwIO e
                    )
                commitResult <- Hasql.run commitSession conn
                Hasql.release conn
                case commitResult of
                    Left err -> throwIO (HasqlSessionError err)
                    Right () -> pure val
    pure result
{-# INLINABLE withTransaction #-}

-- | Build hasql connection settings from a database URL
connectionSettingsFromDatabaseUrl :: ByteString -> [HasqlSetting.Setting]
connectionSettingsFromDatabaseUrl databaseUrl =
    [HasqlSetting.connection (HasqlConnection.string (cs databaseUrl))]
{-# INLINABLE connectionSettingsFromDatabaseUrl #-}

-- | Acquires a new hasql connection using the database URL from ModelContext
acquireConnection :: ModelContext -> IO Hasql.Connection
acquireConnection modelContext = do
    result <- Hasql.acquire (connectionSettingsFromDatabaseUrl modelContext.databaseUrl)
    case result of
        Left err -> throwIO (userError ("Failed to acquire connection: " <> show err))
        Right conn -> pure conn

-- | Executes the given block with the main database role and temporarly sidesteps the row level security policies.
withRowLevelSecurityDisabled :: (?modelContext :: ModelContext) => ((?modelContext :: ModelContext) => IO a) -> IO a
withRowLevelSecurityDisabled block = do
    let currentModelContext = ?modelContext
    let ?modelContext = currentModelContext { rowLevelSecurity = Nothing } in block
{-# INLINABLE withRowLevelSecurityDisabled #-}

-- | Returns the hasql connection when called within a 'withTransaction' block
--
-- Throws an error if called from outside a 'withTransaction'
transactionConnectionOrError :: (?modelContext :: ModelContext) => Hasql.Connection
transactionConnectionOrError = ?modelContext.transactionConnection
            |> \case
                Just connection -> connection
                Nothing -> error "transactionConnectionOrError: Not in a transaction state"

commitTransaction :: (?modelContext :: ModelContext) => IO ()
commitTransaction = do
    let conn = transactionConnectionOrError
    result <- Hasql.run (Hasql.sql "COMMIT") conn
    case result of
        Left err -> throwIO (HasqlSessionError err)
        Right () -> pure ()
{-# INLINABLE commitTransaction #-}

rollbackTransaction :: (?modelContext :: ModelContext) => IO ()
rollbackTransaction = do
    let conn = transactionConnectionOrError
    result <- Hasql.run (Hasql.sql "ROLLBACK") conn
    case result of
        Left err -> throwIO (HasqlSessionError err)
        Right () -> pure ()
{-# INLINABLE rollbackTransaction #-}


-- | Access meta data for a database table
class
    ( KnownSymbol (GetTableName record)
    ) => Table record where
    -- | Returns the table name of a given model.
    --
    -- __Example:__
    --
    -- >>> tableName @User
    -- "users"
    --

    tableName :: Text
    tableName = symbolToText @(GetTableName record)
    {-# INLINE tableName #-}

    -- | Returns the table name of a given model as a bytestring.
    --
    -- __Example:__
    --
    -- >>> tableNameByteString @User
    -- "users"
    --
    tableNameByteString :: ByteString
    tableNameByteString = symbolToByteString @(GetTableName record)
    {-# INLINE tableNameByteString #-}

    -- | Returns the list of column names for a given model
    --
    -- __Example:__
    --
    -- >>> columnNames @User
    -- ["id", "email", "created_at"]
    --
    columnNames :: [ByteString]

    -- | Returns the list of column names, that are contained in the primary key for a given model
    --
    -- __Example:__
    --
    -- >>> primaryKeyColumnNames @User
    -- ["id"]
    --
    -- >>> primaryKeyColumnNames @PostTagging
    -- ["post_id", "tag_id"]
    --
    primaryKeyColumnNames :: [ByteString]

    -- | Returns a Snippet representing the primary key value for use in WHERE conditions
    --
    -- For tables with a simple primary key this simply the id as a param:
    --
    -- >>> primaryKeyConditionForId project.id
    -- param "d619f3cf-f355-4614-8a4c-e9ea4f301e39"
    --
    -- The order of the elements for a composite primary key must match the order of the columns returned by 'primaryKeyColumnNames'
    primaryKeyConditionForId :: Id record -> Snippet

-- | Returns ByteString, that represents the part of an SQL where clause, that matches on a tuple consisting of all the primary keys
-- For table with simple primary keys this simply returns the name of the primary key column, without wrapping in a tuple
-- >>> primaryKeyColumnSelector @PostTag
-- "(post_tags.post_id, post_tags.tag_id)"
-- >>> primaryKeyColumnSelector @Post
-- "post_tags.post_id"
primaryKeyConditionColumnSelector :: forall record. (Table record) => ByteString
primaryKeyConditionColumnSelector =
    let
        qualifyColumnName col = tableNameByteString @record <> "." <> col
    in
    case primaryKeyColumnNames @record of
            [] -> error . cs $ "Impossible happened in primaryKeyConditionColumnSelector. No primary keys found for table " <> tableName @record <> ". At least one primary key is required."
            [s] -> qualifyColumnName s
            conds -> "(" <> BS8.intercalate ", " (map qualifyColumnName conds) <> ")"

-- | Returns WHERE conditions to match an entity by it's primary key
primaryKeyCondition :: forall record. (HasField "id" record (Id record), Table record) => record -> Snippet
primaryKeyCondition record = primaryKeyConditionForId @record record.id

logQuery :: (?modelContext :: ModelContext) => Text -> Text -> NominalDiffTime -> IO ()
logQuery logPrefix queryText time = do
        let ?context = ?modelContext
        let queryTimeInMs = (time * 1000) |> toRational |> fromRational @Double |> round
        let rlsInfo = case ?context.rowLevelSecurity of
                Just RowLevelSecurityContext { rlsUserId } -> " { ihp_user_id = <rls_user> }"
                Nothing -> ""

        Log.debug (logPrefix <> " " <> queryText <> rlsInfo <> " (" <> Text.pack (show queryTimeInMs) <> "ms)")
{-# INLINABLE logQuery #-}

-- | Runs a @DELETE@ query for a record.
--
-- >>> let project :: Project = ...
-- >>> deleteRecord project
-- DELETE FROM projects WHERE id = '..'
--
-- Use 'deleteRecords' if you want to delete multiple records.
--
deleteRecord :: forall record table. (?modelContext :: ModelContext, Table record, Show (PrimaryKey table), HasField "id" record (Id record), GetTableName record ~ table, record ~ GetModelByTableName table) => record -> IO ()
deleteRecord record =
    deleteRecordById @record record.id
{-# INLINABLE deleteRecord #-}

-- | Like 'deleteRecord' but using an Id
deleteRecordById :: forall record table. (?modelContext :: ModelContext, Table record, Show (PrimaryKey table), GetTableName record ~ table, record ~ GetModelByTableName table) => Id' table -> IO ()
deleteRecordById id = do
    let theSnippet = "DELETE FROM " <> Snippet.sql (cs (tableNameByteString @record)) <> " WHERE " <> Snippet.sql (cs (primaryKeyConditionColumnSelector @record)) <> " = " <> primaryKeyConditionForId @record id
    sqlExec theSnippet
    pure ()
{-# INLINABLE deleteRecordById #-}

-- | Runs a @DELETE@ query for a list of records.
--
-- >>> let projects :: [Project] = ...
-- >>> deleteRecords projects
-- DELETE FROM projects WHERE id IN (..)
deleteRecords :: forall record table. (?modelContext :: ModelContext, Show (PrimaryKey table), Table record, HasField "id" record (Id' table), GetTableName record ~ table, record ~ GetModelByTableName table) => [record] -> IO ()
deleteRecords records =
    deleteRecordByIds @record (ids records)
{-# INLINABLE deleteRecords #-}

-- | Like 'deleteRecordById' but for a list of Ids.
deleteRecordByIds :: forall record table. (?modelContext :: ModelContext, Show (PrimaryKey table), Table record, GetTableName record ~ table, record ~ GetModelByTableName table) => [Id' table] -> IO ()
deleteRecordByIds ids = do
    let pkSnippets = map (primaryKeyConditionForId @record) ids
    let inList = mconcat $ intersperse ", " pkSnippets
    let theSnippet = "DELETE FROM " <> Snippet.sql (cs (tableNameByteString @record)) <> " WHERE " <> Snippet.sql (cs (primaryKeyConditionColumnSelector @record)) <> " IN (" <> inList <> ")"
    sqlExec theSnippet
    pure ()
  where
    intersperse :: a -> [a] -> [a]
    intersperse _ [] = []
    intersperse _ [x] = [x]
    intersperse sep (x:xs) = x : sep : intersperse sep xs
{-# INLINABLE deleteRecordByIds #-}

-- | Runs a @DELETE@ query to delete all rows in a table.
--
-- >>> deleteAll @Project
-- DELETE FROM projects
deleteAll :: forall record. (?modelContext :: ModelContext, Table record) => IO ()
deleteAll = do
    let theSnippet = "DELETE FROM " <> Snippet.sql (cs (tableName @record :: Text))
    sqlExec theSnippet
    pure ()
{-# INLINABLE deleteAll #-}

instance Default NominalDiffTime where
    def = 0

instance Default TimeOfDay where
    def = TimeOfDay 0 0 0

instance Default LocalTime where
    def = LocalTime def def

instance Default Day where
    def = ModifiedJulianDay 0

instance Default UTCTime where
    def = UTCTime def 0

instance Default ByteString where
    def = ""

instance Default PGInterval where
    def = PGInterval "00:00:00"

class Record model where
    newRecord :: model

-- | Returns the ids for a list of models
--
-- Shorthand for @map (.id) records@.
--
-- >>> users <- query @User |> fetch
-- >>> ids users
-- [227fbba3-0578-4eb8-807d-b9b692c3644f, 9d7874f2-5343-429b-bcc4-8ee62a5a6895, ...] :: [Id User]
ids :: (HasField "id" record id) => [record] -> [id]
ids records = map (.id) records
{-# INLINE ids #-}

instance Default MetaBag where
    def = MetaBag { annotations = [], touchedFields = [], originalDatabaseRecord = Nothing }
    {-# INLINE def #-}

instance SetField "annotations" MetaBag [(Text, Violation)] where
    setField value meta = meta { annotations = value }
    {-# INLINE setField #-}

instance SetField "touchedFields" MetaBag [Text] where
    setField value meta = meta { touchedFields = value }
    {-# INLINE setField #-}

-- | Returns 'True' if any fields of the record have unsaved changes
didChangeRecord :: (HasField "meta" record MetaBag) => record -> Bool
didChangeRecord record = isEmpty record.meta.touchedFields

-- | Returns 'True' if the specific field of the record has unsaved changes
didChange :: forall fieldName fieldValue record. (KnownSymbol fieldName, HasField fieldName record fieldValue, HasField "meta" record MetaBag, Eq fieldValue, Typeable record) => Proxy fieldName -> record -> Bool
didChange field record = didTouchField field record && didChangeField
    where
        didChangeField :: Bool
        didChangeField = originalFieldValue /= fieldValue

        fieldValue :: fieldValue
        fieldValue = record |> getField @fieldName

        originalFieldValue :: fieldValue
        originalFieldValue =
            record.meta.originalDatabaseRecord
            |> fromMaybe (error "didChange called on a record without originalDatabaseRecord")
            |> fromDynamic @record
            |> fromMaybe (error "didChange failed to retrieve originalDatabaseRecord")
            |> getField @fieldName

-- | Returns 'True' if 'set' was called on that field
didTouchField :: forall fieldName fieldValue record. (KnownSymbol fieldName, HasField fieldName record fieldValue, HasField "meta" record MetaBag, Eq fieldValue, Typeable record) => Proxy fieldName -> record -> Bool
didTouchField field record =
    record.meta.touchedFields
    |> includes (symbolToText @fieldName)

-- | Construct a 'FieldWithDefault'
--
--   Use the default SQL value when the field hasn't been touched since the
--   record was created. This information is stored in the 'touchedFields'
--   attribute of the 'meta' field.
fieldWithDefault
  :: ( KnownSymbol name
     , HasField name model value
     , HasField "meta" model MetaBag
     )
  => Proxy name
  -> model
  -> FieldWithDefault value
fieldWithDefault name model
  | cs (symbolVal name) `elem` model.meta.touchedFields =
    NonDefault (get name model)
  | otherwise = Default

-- | Construct a 'FieldWithUpdate'
--
--   Use the current database value when the field hasn't been touched since the
--   record was accessed. This information is stored in the 'touchedFields'
--   attribute of the 'meta' field.
fieldWithUpdate
  :: ( KnownSymbol name
    , HasField name model value
    , HasField "meta" model MetaBag
    )
  => Proxy name
  -> model
  -> FieldWithUpdate name value
fieldWithUpdate name model
  | cs (symbolVal name) `elem` model.meta.touchedFields =
    Update (get name model)
  | otherwise = NoUpdate name

instance (ToJSON (PrimaryKey a)) => ToJSON (Id' a) where
  toJSON (Id a) = toJSON a

instance (FromJSON (PrimaryKey a)) => FromJSON (Id' a) where
    parseJSON value = Id <$> parseJSON value

instance Default Aeson.Value where
    def = Aeson.Null

-- | Useful to manually mark a table read when doing a custom sql query inside AutoRefresh or 'withTableReadTracker'.
--
-- When using 'fetch' on a query builder, this function is automatically called. That's why you only need to call
-- it yourself when using 'sqlQuery' to run a custom query.
--
-- __Example:__
--
-- > action MyAction = autoRefresh do
-- >     users <- sqlQuery "SELECT * FROM users WHERE .."
-- >     trackTableRead "users"
-- >
-- >     render MyView { .. }
--
--
trackTableRead :: (?modelContext :: ModelContext) => ByteString -> IO ()
trackTableRead tableName = case ?modelContext.trackTableReadCallback of
    Just callback -> callback tableName
    Nothing -> pure ()
{-# INLINABLE trackTableRead #-}

-- | Track all tables in SELECT queries executed within the given IO action.
withTableReadTracker :: (?modelContext :: ModelContext) => ((?modelContext :: ModelContext, ?touchedTables :: IORef (Set.Set ByteString)) => IO ()) -> IO ()
withTableReadTracker trackedSection = do
    touchedTablesVar <- newIORef Set.empty
    let trackTableReadCallback = Just \tableName -> modifyIORef' touchedTablesVar (Set.insert tableName)
    let oldModelContext = ?modelContext
    let ?modelContext = oldModelContext { trackTableReadCallback }
    let ?touchedTables = touchedTablesVar
    trackedSection


-- | Shorthand filter function
onlyWhere :: forall record fieldName value. (KnownSymbol fieldName, HasField fieldName record value, Eq value) => Proxy fieldName -> value -> [record] -> [record]
onlyWhere field value records = filter (\record -> get field record == value) records

-- | Shorthand filter function for Id fields
onlyWhereReferences :: forall record fieldName value referencedRecord. (KnownSymbol fieldName, HasField fieldName record value, Eq value, HasField "id" referencedRecord value) => Proxy fieldName -> referencedRecord -> [record] -> [record]
onlyWhereReferences field referenced records = filter (\record -> get field record == referenced.id) records

-- | Shorthand filter function for nullable Id fields
onlyWhereReferencesMaybe :: forall record fieldName value referencedRecord. (KnownSymbol fieldName, HasField fieldName record (Maybe value), Eq value, HasField "id" referencedRecord value) => Proxy fieldName -> referencedRecord -> [record] -> [record]
onlyWhereReferencesMaybe field referenced records = filter (\record -> get field record == Just referenced.id) records

-- | Returns True when a record has no validation errors attached from a previous validation call
isValid :: forall record. (HasField "meta" record MetaBag) => record -> Bool
isValid record = isEmpty record.meta.annotations

-- | Copies all the fields (except the 'id' field) into a new record
copyRecord :: forall record id. (Table record, SetField "id" record id, Default id, SetField "meta" record MetaBag) => record -> record
copyRecord existingRecord =
    let
        fieldsExceptId = (columnNames @record) |> filter (\field -> field /= "id")

        meta :: MetaBag
        meta = def { touchedFields = map (IHP.NameSupport.columnNameToFieldName . cs) fieldsExceptId }
    in
        existingRecord
            |> set #id def
            |> set #meta meta

-- | Runs sql queries without logging them
withoutQueryLogging :: (?modelContext :: ModelContext) => ((?modelContext :: ModelContext) => result) -> result
withoutQueryLogging callback =
    let
        modelContext = ?modelContext
        nullLogger = modelContext.logger { write = \_ -> pure ()}
    in
        let ?modelContext = modelContext { logger = nullLogger }
        in
            callback

-- FromField instances for custom Postgres types
-- These bridge the ihp-postgresql-simple-extra types with hasql decoders

instance FromField Point where
    fromField = Decoders.custom \_ bytes ->
        case parseOnly parsePoint bytes of
            Left err -> Left (fromString err)
            Right val -> Right val

instance FromField Polygon where
    fromField = Decoders.custom \_ bytes ->
        case parseOnly parsePolygon bytes of
            Left err -> Left (fromString err)
            Right val -> Right val

instance FromField IP where
    fromField = Decoders.custom \_ bytes ->
        case parseIP bytes of
            Left err -> Left (fromString err)
            Right val -> Right val

instance FromField PGInterval where
    fromField = Decoders.custom \_ bytes -> Right (PGInterval bytes)

instance FromField TSVector where
    fromField = Decoders.custom \_ bytes ->
        case parseOnly parseTSVector bytes of
            Left err -> Left (fromString err)
            Right val -> Right val

-- DefaultParamEncoder instances for types not covered by hasql-implicits
-- hasql-implicits provides instances for: Bool, Double, Float, Scientific,
-- Int16, Int32, Int64, Text, ByteString, UUID, UTCTime, Day, etc.

instance DefaultParamEncoder Int where
    defaultParam = Encoders.nonNullable (contramap fromIntegral Encoders.int8)

-- DefaultParamEncoder instances for custom Postgres types

instance DefaultParamEncoder Point where
    defaultParam = Encoders.nonNullable (contramap pointToText Encoders.text)

instance DefaultParamEncoder Polygon where
    defaultParam = Encoders.nonNullable (contramap polygonToText Encoders.text)

instance DefaultParamEncoder IP where
    defaultParam = Encoders.nonNullable (contramap IP.encode Encoders.text)

instance DefaultParamEncoder PGInterval where
    defaultParam = Encoders.nonNullable (contramap (\(PGInterval bs) -> Text.decodeUtf8 bs) Encoders.text)

instance DefaultParamEncoder TSVector where
    defaultParam = Encoders.nonNullable (contramap tsvectorToText Encoders.text)

-- FromField instances for common hasql types (bridging our custom class with hasql decoders)
instance FromField UUID where
    fromField = Decoders.uuid

instance FromField Text where
    fromField = Decoders.text

instance FromField Bool where
    fromField = Decoders.bool

instance FromField UTCTime where
    fromField = Decoders.timestamptz

instance FromField Day where
    fromField = Decoders.date

instance FromField TimeOfDay where
    fromField = Decoders.time

instance FromField LocalTime where
    fromField = Decoders.timestamp

instance FromField Int where
    fromField = fromIntegral <$> Decoders.int4

instance FromField Integer where
    fromField = fromIntegral <$> Decoders.int8

instance FromField Double where
    fromField = Decoders.float8

instance FromField Float where
    fromField = Decoders.float4

instance FromField Scientific where
    fromField = Decoders.numeric

instance FromField ByteString where
    fromField = Decoders.bytea

instance FromField Aeson.Value where
    fromField = Decoders.jsonb
