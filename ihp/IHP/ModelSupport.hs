{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts, AllowAmbiguousTypes, UndecidableInstances, FlexibleInstances, IncoherentInstances, DataKinds, PolyKinds, TypeApplications, ScopedTypeVariables, ConstraintKinds, TypeOperators, GADTs, GeneralizedNewtypeDeriving, CPP #-}

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
import Data.Int (Int64)
import Data.IORef (IORef, newIORef, modifyIORef')
import Data.Hashable (Hashable)
import Control.DeepSeq (NFData)
import Control.Exception (finally, throwIO, Exception, SomeException, try, mask)
import Data.Maybe (fromMaybe, isNothing, isJust)
import Data.List (filter, elem)
import qualified Data.ByteString.Char8 as BS8
import Data.String (IsString(..))
import Database.PostgreSQL.Simple.Types (Query(..))
import Database.PostgreSQL.Simple.FromField hiding (Field, name)
import Database.PostgreSQL.Simple.ToField
import Data.Default
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.String.Conversions (cs ,ConvertibleStrings)
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Calendar
import Data.UUID
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Types as PG
import qualified Database.PostgreSQL.Simple.FromRow as PGFR
import qualified Database.PostgreSQL.Simple.ToField as PG
import qualified Database.PostgreSQL.Simple.ToRow as PG
import GHC.Records
import GHC.TypeLits
import GHC.Types
import Data.Proxy
import Data.Data
import Data.Aeson (ToJSON (..), FromJSON (..))
import qualified Data.Aeson as Aeson
import qualified Data.Set as Set
import qualified Text.Read as Read
import qualified Hasql.Pool as HasqlPool
import qualified Hasql.Pool.Config as HasqlPoolConfig
import qualified Hasql.Connection.Settings as HasqlSettings
import qualified Hasql.Session as Hasql
import qualified Hasql.Statement as Hasql
import qualified Hasql.Errors as HasqlErrors
import qualified Hasql.DynamicStatements.Snippet as Snippet
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Implicits.Encoders
import IHP.Postgres.Point
import IHP.Postgres.Interval ()
import IHP.Postgres.Polygon
import IHP.Postgres.Inet ()
import IHP.Postgres.TSVector
import IHP.Postgres.TimeParser
import IHP.Log.Types
import qualified IHP.Log as Log
import Data.Dynamic
import IHP.EnvVar
import Data.Scientific
import GHC.Stack
import qualified Numeric
import qualified Data.Text.Encoding as Text
import qualified Hasql.Transaction as Tx
import qualified Hasql.Transaction.Sessions as Tx
import Data.Functor.Contravariant (contramap)
import Control.Concurrent (forkIO, MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Error.Class (catchError)
import qualified Hasql.Errors as HasqlErrors
import IHP.Hasql.FromRow (FromRowHasql(..), HasqlDecodeColumn(..))
import IHP.Hasql.Encoders (ToSnippetParams(..), sqlToSnippet)

-- | Provides a mock ModelContext to be used when a database connection is not available
notConnectedModelContext :: Logger -> ModelContext
notConnectedModelContext logger = ModelContext
    { hasqlPool = error "Not connected"
    , transactionRunner = Nothing
    , logger = logger
    , trackTableReadCallback = Nothing
    , rowLevelSecurity = Nothing
    }

createModelContext :: NominalDiffTime -> Int -> ByteString -> Logger -> IO ModelContext
createModelContext idleTime maxConnections databaseUrl logger = do
    -- Create hasql pool for prepared statement-based queries
    -- HASQL_POOL_SIZE: pool size (default: maxConnections). Set to 1 for consistent prepared statement caching.
    -- HASQL_IDLE_TIME: seconds before idle connection is closed (default: 600 = 10 min)
    hasqlPoolSize :: Maybe Int <- envOrNothing "HASQL_POOL_SIZE"
    hasqlIdleTime :: Maybe Int <- envOrNothing "HASQL_IDLE_TIME"
    let hasqlPoolSettings =
            [ HasqlPoolConfig.staticConnectionSettings (HasqlSettings.connectionString (cs databaseUrl))
            ]
            <> maybe [HasqlPoolConfig.size maxConnections] (\size -> [HasqlPoolConfig.size size]) hasqlPoolSize
            <> maybe [] (\idle -> [HasqlPoolConfig.idlenessTimeout (fromIntegral idle)]) hasqlIdleTime
    let hasqlPoolConfig = HasqlPoolConfig.settings hasqlPoolSettings
    hasqlPool <- HasqlPool.acquire hasqlPoolConfig

    let trackTableReadCallback = Nothing
    let transactionRunner = Nothing
    let rowLevelSecurity = Nothing
    pure ModelContext { .. }

releaseModelContext :: ModelContext -> IO ()
releaseModelContext modelContext = do
    HasqlPool.release modelContext.hasqlPool

{-# INLINE createRecord #-}
createRecord :: (?modelContext :: ModelContext, CanCreate model) => model -> IO model
createRecord = create

instance Default Text where
    {-# INLINE def #-}
    def = ""

#if !MIN_VERSION_data_default(0,8,0)
instance Default Bool where
    {-# INLINE def #-}
    def = False
#endif

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

instance FromField (PrimaryKey model) => FromField (Id' model) where
    {-# INLINE fromField #-}
    fromField value metaData = do
        fieldValue <- fromField value metaData
        pure (Id fieldValue)

instance ToField (PrimaryKey model) => ToField (Id' model) where
    {-# INLINE toField #-}
    toField = toField . unpackId

-- | ToField instance for composite primary keys (tuples of two Id' types)
-- Used by filterWhereIdIn for tables with composite primary keys
instance (ToField (Id' a), ToField (Id' b)) => ToField (Id' a, Id' b) where
    {-# INLINE toField #-}
    toField (a, b) = PG.Many [PG.Plain "(", toField a, PG.Plain ",", toField b, PG.Plain ")"]

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

instance (FromField label, PG.FromRow a) => PGFR.FromRow (LabeledData label a) where
    fromRow = LabeledData <$> PGFR.field <*> PGFR.fromRow

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


-- | Runs a raw sql query
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
sqlQuery :: (?modelContext :: ModelContext, ToSnippetParams q, FromRowHasql r) => Query -> q -> IO [r]
sqlQuery theQuery theParameters = do
    let pool = ?modelContext.hasqlPool
    let snippet = sqlToSnippet (fromQuery theQuery) (toSnippetParams theParameters)
    sqlQueryHasql pool snippet (Decoders.rowList hasqlRowDecoder)
{-# INLINABLE sqlQuery #-}


-- | Runs a raw sql query, that is expected to return a single result row
--
-- Like 'sqlQuery', but useful when you expect only a single row as the result
--
-- __Example:__
--
-- > user <- sqlQuerySingleRow "SELECT id, firstname, lastname FROM users WHERE id = ?" (Only user.id)
--
-- Take a look at "IHP.QueryBuilder" for a typesafe approach on building simple queries.
--
-- *AutoRefresh:* When using 'sqlQuerySingleRow' with AutoRefresh, you need to use 'trackTableRead' to let AutoRefresh know that you have accessed a certain table. Otherwise AutoRefresh will not watch table of your custom sql query.
--
sqlQuerySingleRow :: (?modelContext :: ModelContext, ToSnippetParams query, FromRowHasql record) => Query -> query -> IO record
sqlQuerySingleRow theQuery theParameters = do
    result <- sqlQuery theQuery theParameters
    case result of
        [] -> error ("sqlQuerySingleRow: Expected a single row to be returned. Query: " <> show theQuery)
        [record] -> pure record
        otherwise -> error ("sqlQuerySingleRow: Expected a single row to be returned. But got " <> show (length otherwise) <> " rows")
{-# INLINABLE sqlQuerySingleRow #-}

-- | Runs a sql statement (like a CREATE statement)
--
-- __Example:__
--
-- > sqlExec "CREATE TABLE users ()" ()
sqlExec :: (?modelContext :: ModelContext, ToSnippetParams q) => Query -> q -> IO Int64
sqlExec theQuery theParameters = do
    let pool = ?modelContext.hasqlPool
    let snippet = sqlToSnippet (fromQuery theQuery) (toSnippetParams theParameters)
    sqlExecHasqlCount pool snippet
{-# INLINABLE sqlExec #-}

-- | Runs a sql statement (like a CREATE statement), but doesn't return any result
--
-- __Example:__
--
-- > sqlExecDiscardResult "CREATE TABLE users ()" ()
sqlExecDiscardResult :: (?modelContext :: ModelContext, ToSnippetParams q) => Query -> q -> IO ()
sqlExecDiscardResult theQuery theParameters = do
    _ <- sqlExec theQuery theParameters
    pure ()
{-# INLINABLE sqlExecDiscardResult #-}


-- | Prepared statement that sets the RLS role and user id using set_config().
--
-- Uses @set_config(setting, value, is_local)@ which is a regular SQL function
-- that supports parameterized values in the extended query protocol, unlike
-- @SET LOCAL@ which is a utility command that cannot be parameterized.
--
-- The third argument @true@ makes the setting local to the current transaction,
-- equivalent to @SET LOCAL@.
setRLSConfigStatement :: Hasql.Statement (Text, Text) ()
setRLSConfigStatement = Hasql.preparable
    "SELECT set_config('role', $1, true), set_config('rls.ihp_user_id', $2, true)"
    (contramap fst (Encoders.param (Encoders.nonNullable Encoders.text))
     <> contramap snd (Encoders.param (Encoders.nonNullable Encoders.text)))
    (Decoders.singleRow (Decoders.column (Decoders.nullable Decoders.text) *> Decoders.column (Decoders.nullable Decoders.text) *> pure ()))

-- | Runs a query using the hasql pool with prepared statements
--
-- This function executes a query using hasql's prepared statement mechanism,
-- which provides better performance than postgresql-simple for repeated queries.
--
-- When RLS is enabled, the query is wrapped in a transaction that first sets the
-- role and user id via 'setRLSConfigStatement'.
--
-- __Example:__
--
-- > users <- sqlQueryHasql pool snippet (Decoders.rowList userDecoder)
--
sqlQueryHasql :: (?modelContext :: ModelContext) => HasqlPool.Pool -> Snippet.Snippet -> Decoders.Result a -> IO a
sqlQueryHasql pool snippet decoder = do
    let ?context = ?modelContext
    let currentLogLevel = ?modelContext.logger.level
    let statement = Snippet.toStatement snippet decoder
    let session = case (?modelContext.transactionRunner, ?modelContext.rowLevelSecurity) of
            (Just _, _) ->
                -- In transaction: RLS already configured at BEGIN time
                Hasql.statement () statement
            (_, Just RowLevelSecurityContext { rlsAuthenticatedRole, rlsUserId }) ->
                Tx.transaction Tx.ReadCommitted Tx.Read $ do
                    Tx.statement (rlsAuthenticatedRole, rlsUserId) setRLSConfigStatement
                    Tx.statement () statement
            _ ->
                Hasql.statement () statement
    let runQuery = case ?modelContext.transactionRunner of
            Just (TransactionRunner runner) -> runner session
            Nothing -> do
                result <- HasqlPool.use pool session
                case result of
                    Left err
                        | isCachedPlanError err -> do
                            Log.info ("Resetting hasql connection pool due to stale prepared statements (e.g. after 'make db')" :: Text)
                            HasqlPool.release pool
                            retryResult <- HasqlPool.use pool session
                            case retryResult of
                                Left retryErr -> throwIO (HasqlError retryErr)
                                Right a -> pure a
                        | otherwise -> throwIO (HasqlError err)
                    Right a -> pure a
    if currentLogLevel == Debug
        then do
            start <- getCurrentTime
            runQuery `finally` do
                end <- getCurrentTime
                let queryTimeInMs = round (realToFrac (end `diffUTCTime` start) * 1000 :: Double)
                let sqlText = Hasql.toSql statement
                Log.debug ("🔍 " <> truncateQuery (cs sqlText) <> " (" <> Text.pack (show queryTimeInMs) <> "ms)")
        else runQuery
{-# INLINABLE sqlQueryHasql #-}

-- | Like 'sqlQueryHasql' but for statements that don't return results (DELETE, etc.)
--
-- When RLS is enabled, the statement is wrapped in a transaction that first sets the
-- role and user id via 'setRLSConfigStatement'.
sqlExecHasql :: (?modelContext :: ModelContext) => HasqlPool.Pool -> Snippet.Snippet -> IO ()
sqlExecHasql pool snippet = do
    let ?context = ?modelContext
    let currentLogLevel = ?modelContext.logger.level
    let statement = Snippet.toStatement snippet Decoders.noResult
    let session = case (?modelContext.transactionRunner, ?modelContext.rowLevelSecurity) of
            (Just _, _) ->
                Hasql.statement () statement
            (_, Just RowLevelSecurityContext { rlsAuthenticatedRole, rlsUserId }) ->
                Tx.transaction Tx.ReadCommitted Tx.Write $ do
                    Tx.statement (rlsAuthenticatedRole, rlsUserId) setRLSConfigStatement
                    Tx.statement () statement
            _ ->
                Hasql.statement () statement
    let runQuery = case ?modelContext.transactionRunner of
            Just (TransactionRunner runner) -> runner session
            Nothing -> do
                result <- HasqlPool.use pool session
                case result of
                    Left err
                        | isCachedPlanError err -> do
                            Log.info ("Resetting hasql connection pool due to stale prepared statements (e.g. after 'make db')" :: Text)
                            HasqlPool.release pool
                            retryResult <- HasqlPool.use pool session
                            case retryResult of
                                Left retryErr -> throwIO (HasqlError retryErr)
                                Right () -> pure ()
                        | otherwise -> throwIO (HasqlError err)
                    Right () -> pure ()
    if currentLogLevel == Debug
        then do
            start <- getCurrentTime
            runQuery `finally` do
                end <- getCurrentTime
                let queryTimeInMs = round (realToFrac (end `diffUTCTime` start) * 1000 :: Double)
                let sqlText = Hasql.toSql statement
                Log.debug ("💾 " <> truncateQuery (cs sqlText) <> " (" <> Text.pack (show queryTimeInMs) <> "ms)")
        else runQuery
{-# INLINABLE sqlExecHasql #-}

-- | Like 'sqlExecHasql' but returns the number of affected rows
--
-- When RLS is enabled, the statement is wrapped in a transaction that first sets the
-- role and user id via 'setRLSConfigStatement'.
sqlExecHasqlCount :: (?modelContext :: ModelContext) => HasqlPool.Pool -> Snippet.Snippet -> IO Int64
sqlExecHasqlCount pool snippet = do
    let ?context = ?modelContext
    let currentLogLevel = ?modelContext.logger.level
    let statement = Snippet.toStatement snippet Decoders.rowsAffected
    let session = case (?modelContext.transactionRunner, ?modelContext.rowLevelSecurity) of
            (Just _, _) ->
                Hasql.statement () statement
            (_, Just RowLevelSecurityContext { rlsAuthenticatedRole, rlsUserId }) ->
                Tx.transaction Tx.ReadCommitted Tx.Write $ do
                    Tx.statement (rlsAuthenticatedRole, rlsUserId) setRLSConfigStatement
                    Tx.statement () statement
            _ ->
                Hasql.statement () statement
    let runQuery = case ?modelContext.transactionRunner of
            Just (TransactionRunner runner) -> runner session
            Nothing -> do
                result <- HasqlPool.use pool session
                case result of
                    Left err -> throwIO (HasqlError err)
                    Right count -> pure count
    if currentLogLevel == Debug
        then do
            start <- getCurrentTime
            runQuery `finally` do
                end <- getCurrentTime
                let queryTimeInMs = round (realToFrac (end `diffUTCTime` start) * 1000 :: Double)
                let sqlText = Hasql.toSql statement
                Log.debug ("💾 " <> cs sqlText <> " (" <> Text.pack (show queryTimeInMs) <> "ms)")
        else runQuery
{-# INLINABLE sqlExecHasqlCount #-}

-- | Like 'sqlExecHasql' but for raw 'Hasql.Session' values (e.g. multi-statement DDL via 'Hasql.sql')
--
-- Use this instead of 'sqlExecHasql' when you need the simple protocol (no prepared statements),
-- e.g. for multi-statement SQL like trigger creation.
--
-- __Example:__
--
-- > runSessionHasql pool (Hasql.sql "BEGIN; CREATE ...; COMMIT;")
--
runSessionHasql :: (?modelContext :: ModelContext) => HasqlPool.Pool -> Hasql.Session () -> IO ()
runSessionHasql pool session = do
    let ?context = ?modelContext
    let currentLogLevel = ?modelContext.logger.level
    let runQuery = case ?modelContext.transactionRunner of
            Just (TransactionRunner runner) -> runner session
            Nothing -> do
                result <- HasqlPool.use pool session
                case result of
                    Left err
                        | isCachedPlanError err -> do
                            Log.info ("Resetting hasql connection pool due to stale prepared statements (e.g. after 'make db')" :: Text)
                            HasqlPool.release pool
                            retryResult <- HasqlPool.use pool session
                            case retryResult of
                                Left retryErr -> throwIO (HasqlError retryErr)
                                Right () -> pure ()
                        | otherwise -> throwIO (HasqlError err)
                    Right () -> pure ()
    if currentLogLevel == Debug
        then do
            start <- getCurrentTime
            runQuery `finally` do
                end <- getCurrentTime
                let queryTimeInMs = round (realToFrac (end `diffUTCTime` start) * 1000 :: Double)
                Log.debug ("💾 runSessionHasql (" <> Text.pack (show queryTimeInMs) <> "ms)")
        else runQuery
{-# INLINABLE runSessionHasql #-}


-- | Exception type for hasql errors
data HasqlError = HasqlError HasqlPool.UsageError
    deriving (Show)

instance Exception HasqlError

-- | Existential wrapper for sub-session requests in a transaction
data SessionRequest where
    SessionRequest :: Hasql.Session a -> MVar (Either HasqlErrors.SessionError a) -> SessionRequest

-- | Loop that reads sub-session requests from an MVar and executes them
-- on the current transaction's connection. Stops when it receives 'Nothing'.
processRequests :: MVar (Maybe SessionRequest) -> Hasql.Session ()
processRequests requestMVar = do
    req <- liftIO (takeMVar requestMVar)
    case req of
        Just (SessionRequest session responseVar) -> do
            result <- catchError (Right <$> session) (pure . Left)
            liftIO (putMVar responseVar result)
            processRequests requestMVar
        Nothing -> pure ()

-- | Detects errors caused by stale schema after @make db@ recreates the database.
--
-- Matches four categories:
--
-- 1. PostgreSQL \"cached plan must not change result type\" (error code 0A000) —
--    the server rejects a prepared statement whose result columns changed.
--
-- 2. PostgreSQL \"cache lookup failed for type\" (error code XX000) —
--    a prepared statement references a type OID that no longer exists after
--    schema recreation (types get new OIDs).
--
-- 3. Hasql 'MissingTypesSessionError' — custom enum types (e.g. @JOB_STATUS@)
--    get new OIDs after schema recreation, and hasql's type registry can't find them.
--
-- 4. Hasql 'UnexpectedColumnTypeStatementError' — the column's type OID no longer
--    matches the OID cached in the prepared statement / decoder.
isCachedPlanError :: HasqlPool.UsageError -> Bool
isCachedPlanError (HasqlPool.SessionUsageError sessionError) = isCachedPlanSessionError sessionError
isCachedPlanError _ = False

isCachedPlanSessionError :: HasqlErrors.SessionError -> Bool
isCachedPlanSessionError (HasqlErrors.StatementSessionError _ _ _ _ _ (HasqlErrors.ServerStatementError (HasqlErrors.ServerError "0A000" _ _ _ _))) = True
isCachedPlanSessionError (HasqlErrors.StatementSessionError _ _ _ _ _ (HasqlErrors.ServerStatementError (HasqlErrors.ServerError "XX000" _ _ _ _))) = True
isCachedPlanSessionError (HasqlErrors.ScriptSessionError _ (HasqlErrors.ServerError "0A000" _ _ _ _)) = True
isCachedPlanSessionError (HasqlErrors.ScriptSessionError _ (HasqlErrors.ServerError "XX000" _ _ _ _)) = True
isCachedPlanSessionError (HasqlErrors.MissingTypesSessionError _) = True
isCachedPlanSessionError (HasqlErrors.StatementSessionError _ _ _ _ _ (HasqlErrors.UnexpectedColumnTypeStatementError _ _ _)) = True
isCachedPlanSessionError _ = False

-- | Runs a raw sql query which results in a single scalar value such as an integer or string
--
-- __Example:__
--
-- > usersCount <- sqlQueryScalar "SELECT COUNT(*) FROM users"
--
-- Take a look at "IHP.QueryBuilder" for a typesafe approach on building simple queries.
sqlQueryScalar :: (?modelContext :: ModelContext, ToSnippetParams q, FromField value, HasqlDecodeColumn value) => Query -> q -> IO value
sqlQueryScalar theQuery theParameters = do
    result <- sqlQuery theQuery theParameters
    pure case result of
        [PG.Only result] -> result
        _ -> error "sqlQueryScalar: Expected a scalar result value"
{-# INLINABLE sqlQueryScalar #-}

-- | Runs a raw sql query which results in a single scalar value such as an integer or string, or nothing
--
-- __Example:__
--
-- > usersCount <- sqlQueryScalarOrNothing "SELECT COUNT(*) FROM users"
--
-- Take a look at "IHP.QueryBuilder" for a typesafe approach on building simple queries.
sqlQueryScalarOrNothing :: (?modelContext :: ModelContext, ToSnippetParams q, FromField value, HasqlDecodeColumn value) => Query -> q -> IO (Maybe value)
sqlQueryScalarOrNothing theQuery theParameters = do
    result <- sqlQuery theQuery theParameters
    pure case result of
        [] -> Nothing
        [PG.Only result] -> Just result
        _ -> error "sqlQueryScalarOrNothing: Expected a scalar result value or an empty result set"
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
withTransaction block
    | isJust ?modelContext.transactionRunner =
        error "withTransaction: Nested transactions are not supported. withTransaction was called inside an existing transaction."
    | otherwise = do
    let pool = ?modelContext.hasqlPool
    requestMVar <- newEmptyMVar

    let runner :: forall a. Hasql.Session a -> IO a
        runner session = do
            responseVar <- newEmptyMVar
            putMVar requestMVar (Just (SessionRequest session responseVar))
            result <- takeMVar responseVar
            case result of
                Left err -> throwIO (HasqlSessionError err)
                Right a -> pure a

    let ?modelContext = ?modelContext { transactionRunner = Just (TransactionRunner runner) }

    let ?context = ?modelContext
    let transactionSession = do
            Hasql.script "BEGIN"
            case ?modelContext.rowLevelSecurity of
                Just RowLevelSecurityContext { rlsAuthenticatedRole, rlsUserId } ->
                    Hasql.statement (rlsAuthenticatedRole, rlsUserId) setRLSConfigStatement
                Nothing -> pure ()

            -- Fork the user's block in a separate thread
            blockResultVar <- liftIO $ do
                resultVar <- newEmptyMVar
                _ <- forkIO $ mask \restore -> do
                    result <- try @SomeException (restore block)
                    putMVar requestMVar Nothing   -- Signal processRequests to stop
                    putMVar resultVar result
                pure resultVar

            processRequests requestMVar

            blockResult <- liftIO (takeMVar blockResultVar)
            case blockResult of
                Left exc -> do
                    catchError (Hasql.script "ROLLBACK") (\rollbackErr -> liftIO $
                        Log.warn ("withTransaction: ROLLBACK failed: " <> Text.pack (show rollbackErr)))
                    liftIO (throwIO exc)
                Right a -> do
                    Hasql.script "COMMIT"
                    pure a

    result <- HasqlPool.use pool transactionSession
    case result of
        Left err -> throwIO (HasqlError err)
        Right a -> pure a
{-# INLINABLE withTransaction #-}

-- | Executes the given block with the main database role and temporarly sidesteps the row level security policies.
--
-- This is used e.g. by IHP AutoRefresh to be able to set up it's database triggers. When trying to set up a database
-- trigger from the ihp_authenticated role, it typically fails because it's missing permissions. Using 'withRowLevelSecurityDisabled'
-- we switch to the main role which is allowed to set up database triggers.
--
-- SQL queries run from within the passed block are executed in their own transaction.
--
-- __Example:__
--
-- > -- SQL code executed here might be run from the ihp_authenticated role
-- > withRowLevelSecurityDisabled do
-- >    -- SQL code executed here is run as the main IHP db role
-- >    sqlExec "CREATE OR REPLACE FUNCTION .." ()
--
withRowLevelSecurityDisabled :: (?modelContext :: ModelContext) => ((?modelContext :: ModelContext) => IO a) -> IO a
withRowLevelSecurityDisabled block = do
    let currentModelContext = ?modelContext
    let ?modelContext = currentModelContext { rowLevelSecurity = Nothing } in block
{-# INLINABLE withRowLevelSecurityDisabled #-}

commitTransaction :: (?modelContext :: ModelContext) => IO ()
commitTransaction = case ?modelContext.transactionRunner of
    Just (TransactionRunner runner) -> runner (Hasql.script "COMMIT")
    Nothing -> error "commitTransaction: Not in a transaction"
{-# INLINABLE commitTransaction #-}

rollbackTransaction :: (?modelContext :: ModelContext) => IO ()
rollbackTransaction = case ?modelContext.transactionRunner of
    Just (TransactionRunner runner) -> runner (Hasql.script "ROLLBACK")
    Nothing -> error "rollbackTransaction: Not in a transaction"
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

    -- | Returns the list of column names for a given model
    --
    -- __Example:__
    --
    -- >>> columnNames @User
    -- ["id", "email", "created_at"]
    --
    columnNames :: [Text]

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
    primaryKeyColumnNames :: [Text]


-- | Returns ByteString, that represents the part of an SQL where clause, that matches on a tuple consisting of all the primary keys
-- For table with simple primary keys this simply returns the name of the primary key column, without wrapping in a tuple
-- >>> primaryKeyColumnSelector @PostTag
-- "(post_tags.post_id, post_tags.tag_id)"
-- >>> primaryKeyColumnSelector @Post
-- "post_tags.post_id"
primaryKeyConditionColumnSelector :: forall record. (Table record) => Text
primaryKeyConditionColumnSelector =
    let
        qualifyColumnName col = tableName @record <> "." <> col
    in
    case primaryKeyColumnNames @record of
            [] -> error . cs $ "Impossible happened in primaryKeyConditionColumnSelector. No primary keys found for table " <> tableName @record <> ". At least one primary key is required."
            [s] -> qualifyColumnName s
            conds -> "(" <> Text.intercalate ", " (map qualifyColumnName conds) <> ")"


truncateQuery :: Text -> Text
truncateQuery query
    | Text.length query > 2000 = Text.take 2000 query <> "... (truncated)"
    | otherwise = query

-- | Runs a @DELETE@ query for a record.
--
-- >>> let project :: Project = ...
-- >>> deleteRecord project
-- DELETE FROM projects WHERE id = '..'
--
-- Use 'deleteRecords' if you want to delete multiple records.
--
deleteRecord :: forall record table. (?modelContext :: ModelContext, Table record, Show (PrimaryKey table), HasField "id" record (Id record), GetTableName record ~ table, record ~ GetModelByTableName table, Hasql.Implicits.Encoders.DefaultParamEncoder (Id' table)) => record -> IO ()
deleteRecord record =
    deleteRecordById @record record.id
{-# INLINABLE deleteRecord #-}

-- | Like 'deleteRecord' but using an Id
--
-- >>> let project :: Id Project = ...
-- >>> delete projectId
-- DELETE FROM projects WHERE id = '..'
--
deleteRecordById :: forall record table. (?modelContext :: ModelContext, Table record, Show (PrimaryKey table), GetTableName record ~ table, record ~ GetModelByTableName table, Hasql.Implicits.Encoders.DefaultParamEncoder (Id' table)) => Id' table -> IO ()
deleteRecordById id = do
    let pool = ?modelContext.hasqlPool
    sqlExecHasql pool $
        Snippet.sql ("DELETE FROM " <> tableName @record <> " WHERE " <> primaryKeyConditionColumnSelector @record <> " = ")
        <> Snippet.param id
{-# INLINABLE deleteRecordById #-}

-- | Runs a @DELETE@ query for a list of records.
--
-- >>> let projects :: [Project] = ...
-- >>> deleteRecords projects
-- DELETE FROM projects WHERE id IN (..)
deleteRecords :: forall record table. (?modelContext :: ModelContext, Show (PrimaryKey table), Table record, HasField "id" record (Id' table), GetTableName record ~ table, record ~ GetModelByTableName table, Hasql.Implicits.Encoders.DefaultParamEncoder [Id' table]) => [record] -> IO ()
deleteRecords records =
    deleteRecordByIds @record (ids records)
{-# INLINABLE deleteRecords #-}

-- | Like 'deleteRecordById' but for a list of Ids.
--
-- >>> let projectIds :: [ Id Project ] = ...
-- >>> delete projectIds
-- DELETE FROM projects WHERE id IN ('..')
--
deleteRecordByIds :: forall record table. (?modelContext :: ModelContext, Show (PrimaryKey table), Table record, GetTableName record ~ table, record ~ GetModelByTableName table, Hasql.Implicits.Encoders.DefaultParamEncoder [Id' table]) => [Id' table] -> IO ()
deleteRecordByIds ids = do
    let pool = ?modelContext.hasqlPool
    sqlExecHasql pool $
        Snippet.sql ("DELETE FROM " <> tableName @record <> " WHERE " <> primaryKeyConditionColumnSelector @record <> " = ANY(")
        <> Snippet.param ids
        <> Snippet.sql ")"
{-# INLINABLE deleteRecordByIds #-}

-- | Runs a @DELETE@ query to delete all rows in a table.
--
-- >>> deleteAll @Project
-- DELETE FROM projects
deleteAll :: forall record. (?modelContext :: ModelContext, Table record) => IO ()
deleteAll = do
    let pool = ?modelContext.hasqlPool
    sqlExecHasql pool $ Snippet.sql ("DELETE FROM " <> tableName @record)
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

instance Default (PG.Binary ByteString) where
    def = PG.Binary ""

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
--
-- __Example:__ Returns 'False' for freshly fetched records
--
-- >>> let projectId = "227fbba3-0578-4eb8-807d-b9b692c3644f" :: Id Project
-- >>> project <- fetch projectId
-- >>> didChangeRecord project
-- False
--
-- __Example:__ Returns 'True' after setting a field
--
-- >>> let projectId = "227fbba3-0578-4eb8-807d-b9b692c3644f" :: Id Project
-- >>> project <- fetch projectId
-- >>> project |> set #name "New Name" |> didChangeRecord
-- True
didChangeRecord :: (HasField "meta" record MetaBag) => record -> Bool
didChangeRecord record = isEmpty record.meta.touchedFields

-- | Returns 'True' if the specific field of the record has unsaved changes
--
-- __Example:__ Returns 'False' for freshly fetched records
--
-- >>> let projectId = "227fbba3-0578-4eb8-807d-b9b692c3644f" :: Id Project
-- >>> project <- fetch projectId
-- >>> didChange #name project
-- False
--
-- __Example:__ Returns 'True' after setting a field
--
-- >>> let projectId = "227fbba3-0578-4eb8-807d-b9b692c3644f" :: Id Project
-- >>> project <- fetch projectId
-- >>> project |> set #name "New Name" |> didChange #name
-- True
--
-- __Example:__ Setting a flash message after updating the profile picture
--
-- > when (user |> didChange #profilePictureUrl) (setSuccessMessage "Your Profile Picture has been updated. It might take a few minutes until it shows up everywhere")
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
--
-- __Example:__ Returns 'False' for freshly fetched records
--
-- >>> let projectId = "227fbba3-0578-4eb8-807d-b9b692c3644f" :: Id Project
-- >>> project <- fetch projectId
-- >>> didTouchField #name project
-- False
--
-- __Example:__ Returns 'True' after setting a field
--
-- >>> let projectId = "227fbba3-0578-4eb8-807d-b9b692c3644f" :: Id Project
-- >>> project <- fetch projectId
-- >>> project |> set #name project.name |> didTouchField #name
-- True
--
didTouchField :: forall fieldName fieldValue record. (KnownSymbol fieldName, HasField fieldName record fieldValue, HasField "meta" record MetaBag, Eq fieldValue, Typeable record) => Proxy fieldName -> record -> Bool
didTouchField field record =
    record.meta.touchedFields
    |> includes (symbolToText @fieldName)

instance ToField valueType => ToField (FieldWithDefault valueType) where
  toField Default = Plain "DEFAULT"
  toField (NonDefault a) = toField a

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

instance (KnownSymbol name, ToField value) => ToField (FieldWithUpdate name value) where
  toField (NoUpdate name) =
    Plain (Data.String.fromString $ cs $ fieldNameToColumnName $ cs $ symbolVal name)
  toField (Update a) = toField a

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

-- | Like 'fieldWithDefault' but produces a hasql 'Snippet' instead of a 'FieldWithDefault'
--
--   When the field hasn't been touched, produces @DEFAULT@. Otherwise encodes the value
--   using hasql's 'DefaultParamEncoder'.
fieldWithDefaultSnippet
  :: ( KnownSymbol name
     , HasField name model value
     , HasField "meta" model MetaBag
     , Hasql.Implicits.Encoders.DefaultParamEncoder value
     )
  => Proxy name
  -> model
  -> Snippet.Snippet
fieldWithDefaultSnippet name model
  | cs (symbolVal name) `elem` model.meta.touchedFields = Snippet.param (get name model)
  | otherwise = Snippet.sql "DEFAULT"

-- | Like 'fieldWithUpdate' but produces a hasql 'Snippet' instead of a 'FieldWithUpdate'
--
--   When the field hasn't been touched, produces the column name (keeping the current DB value).
--   Otherwise encodes the new value using hasql's 'DefaultParamEncoder'.
fieldWithUpdateSnippet
  :: ( KnownSymbol name
     , HasField name model value
     , HasField "meta" model MetaBag
     , Hasql.Implicits.Encoders.DefaultParamEncoder value
     )
  => Proxy name
  -> model
  -> Snippet.Snippet
fieldWithUpdateSnippet name model
  | cs (symbolVal name) `elem` model.meta.touchedFields = Snippet.param (get name model)
  | otherwise = Snippet.sql (cs $ fieldNameToColumnName $ cs $ symbolVal name)

instance (ToJSON (PrimaryKey a)) => ToJSON (Id' a) where
  toJSON (Id a) = toJSON a

instance (FromJSON (PrimaryKey a)) => FromJSON (Id' a) where
    parseJSON value = Id <$> parseJSON value


instance Default Aeson.Value where
    def = Aeson.Null

-- | This instance allows us to avoid wrapping lists with PGArray when
-- using sql types such as @INT[]@
instance ToField value => ToField [value] where
    toField list = toField (PG.PGArray list)

-- | This instancs allows us to avoid wrapping lists with PGArray when
-- using sql types such as @INT[]@
instance (FromField value, Typeable value) => FromField [value] where
    fromField field value = PG.fromPGArray <$> (fromField field value)

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
trackTableRead :: (?modelContext :: ModelContext) => Text -> IO ()
trackTableRead tableName = case ?modelContext.trackTableReadCallback of
    Just callback -> callback tableName
    Nothing -> pure ()
{-# INLINABLE trackTableRead #-}

-- | Track all tables in SELECT queries executed within the given IO action.
--
-- You can read the touched tables by this function by accessing the variable @?touchedTables@ inside your given IO action.
--
-- __Example:__
--
-- > withTableReadTracker do
-- >     project <- query @Project |> fetchOne
-- >     user <- query @User |> fetchOne
-- >
-- >     tables <- readIORef ?touchedTables
-- >     -- tables = Set.fromList ["projects", "users"]
-- >
withTableReadTracker :: (?modelContext :: ModelContext) => ((?modelContext :: ModelContext, ?touchedTables :: IORef (Set.Set Text)) => IO ()) -> IO ()
withTableReadTracker trackedSection = do
    touchedTablesVar <- newIORef Set.empty
    let trackTableReadCallback = Just \tableName -> modifyIORef' touchedTablesVar (Set.insert tableName)
    let oldModelContext = ?modelContext
    let ?modelContext = oldModelContext { trackTableReadCallback }
    let ?touchedTables = touchedTablesVar
    trackedSection


-- | Shorthand filter function
--
-- In IHP code bases you often write filter functions such as these:
--
-- > getUserPosts user posts =
-- >     filter (\p -> p.userId == user.id) posts
--
-- This can be written in a shorter way using 'onlyWhere':
--
-- > getUserPosts user posts =
-- >     posts |> onlyWhere #userId user.id
--
-- Because the @userId@ field is an Id, we can use 'onlyWhereReferences' to make it even shorter:
--
-- > getUserPosts user posts =
-- >     posts |> onlyWhereReferences #userId user
--
-- If the Id field is nullable, we need to use 'onlyWhereReferencesMaybe':
--
-- > getUserTasks user tasks =
-- >     tasks |> onlyWhereReferencesMaybe #optionalUserId user
--
onlyWhere :: forall record fieldName value. (KnownSymbol fieldName, HasField fieldName record value, Eq value) => Proxy fieldName -> value -> [record] -> [record]
onlyWhere field value records = filter (\record -> get field record == value) records

-- | Shorthand filter function for Id fields
--
-- In IHP code bases you often write filter functions such as these:
--
-- > getUserPosts user posts =
-- >     filter (\p -> p.userId == user.id) posts
--
-- This can be written in a shorter way using 'onlyWhereReferences':
--
-- > getUserPosts user posts =
-- >     posts |> onlyWhereReferences #userId user
--
-- If the Id field is nullable, we need to use 'onlyWhereReferencesMaybe':
--
-- > getUserTasks user tasks =
-- >     tasks |> onlyWhereReferencesMaybe #optionalUserId user
--
--
-- See 'onlyWhere' for more details.
onlyWhereReferences :: forall record fieldName value referencedRecord. (KnownSymbol fieldName, HasField fieldName record value, Eq value, HasField "id" referencedRecord value) => Proxy fieldName -> referencedRecord -> [record] -> [record]
onlyWhereReferences field referenced records = filter (\record -> get field record == referenced.id) records

-- | Shorthand filter function for nullable Id fields
--
-- In IHP code bases you often write filter functions such as these:
--
-- > getUserTasks user tasks =
-- >     filter (\task -> task.optionalUserId == Just user.id) tasks
--
-- This can be written in a shorter way using 'onlyWhereReferencesMaybe':
--
-- > getUserTasks user tasks =
-- >     tasks |> onlyWhereReferencesMaybe #optionalUserId user
--
-- See 'onlyWhere' for more details.
onlyWhereReferencesMaybe :: forall record fieldName value referencedRecord. (KnownSymbol fieldName, HasField fieldName record (Maybe value), Eq value, HasField "id" referencedRecord value) => Proxy fieldName -> referencedRecord -> [record] -> [record]
onlyWhereReferencesMaybe field referenced records = filter (\record -> get field record == Just referenced.id) records

-- | Returns True when a record has no validation errors attached from a previous validation call
--
-- Example:
--
-- > isValidProject :: Project -> Bool
-- > isValidProject project =
-- >     project
-- >     |> validateField #name isNonEmpty
-- >     |> isValid
--
isValid :: forall record. (HasField "meta" record MetaBag) => record -> Bool
isValid record = isEmpty record.meta.annotations

-- | Copies all the fields (except the 'id' field) into a new record
--
-- Example: Duplicate a database record (except the primary key of course)
--
-- > project <- fetch projectId
-- > duplicatedProject <- createRecord (copyRecord project)
--
copyRecord :: forall record id. (Table record, SetField "id" record id, Default id, SetField "meta" record MetaBag) => record -> record
copyRecord existingRecord =
    let
        fieldsExceptId = (columnNames @record) |> filter (\field -> field /= "id")

        meta :: MetaBag
        meta = def { touchedFields = map IHP.NameSupport.columnNameToFieldName fieldsExceptId }
    in
        existingRecord
            |> set #id def
            |> set #meta meta

-- | Runs sql queries without logging them
--
-- Example:
--
-- > users <- withoutQueryLogging (sqlQuery "SELECT * FROM users" ())
--
withoutQueryLogging :: (?modelContext :: ModelContext) => ((?modelContext :: ModelContext) => result) -> result
withoutQueryLogging callback =
    let
        modelContext = ?modelContext
        nullLogger = modelContext.logger { write = \_ -> pure ()}
    in
        let ?modelContext = modelContext { logger = nullLogger }
        in
            callback