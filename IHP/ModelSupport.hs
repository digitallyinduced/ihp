{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts, AllowAmbiguousTypes, UndecidableInstances, FlexibleInstances, IncoherentInstances, DataKinds, PolyKinds, TypeApplications, ScopedTypeVariables, TypeInType, ConstraintKinds, TypeOperators, GADTs, GeneralizedNewtypeDeriving #-}

module IHP.ModelSupport
( module IHP.ModelSupport
, module IHP.Postgres.Point
, module IHP.Postgres.Polygon
, module IHP.Postgres.Inet
, module IHP.Postgres.TSVector
) where

import IHP.HaskellSupport
import IHP.NameSupport
import ClassyPrelude hiding (UTCTime, find, ModifiedJulianDay)
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.Types (Query)
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
import qualified Data.Pool as Pool
import qualified GHC.Conc
import IHP.Postgres.Point
import IHP.Postgres.Polygon
import IHP.Postgres.Inet ()
import IHP.Postgres.TSVector
import IHP.Log.Types
import qualified IHP.Log as Log
import Data.Dynamic
import Data.Scientific
import GHC.Stack
import qualified Numeric
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString.Builder as Builder

-- | Provides the db connection and some IHP-specific db configuration
data ModelContext = ModelContext
    { connectionPool :: Pool.Pool Connection -- ^ Used to get database connections when no 'transactionConnection' is set
    , transactionConnection :: Maybe Connection -- ^ Set to a specific database connection when executing a database transaction
    -- | Logs all queries to this logger at log level info
    , logger :: Logger
    -- | A callback that is called whenever a specific table is accessed using a SELECT query
    , trackTableReadCallback :: Maybe (ByteString -> IO ())
    -- | Is set to a value if row level security was enabled at runtime
    , rowLevelSecurity :: Maybe RowLevelSecurityContext
    }

-- | When row level security is enabled at runtime, this keeps track of the current
-- logged in user and the postgresql role to switch to.
data RowLevelSecurityContext = RowLevelSecurityContext
    { rlsAuthenticatedRole :: Text -- ^ Default is @ihp_authenticated@. This value comes from the @IHP_RLS_AUTHENTICATED_ROLE@  env var.
    , rlsUserId :: PG.Action -- ^ The user id of the current logged in user
    }

-- | Provides a mock ModelContext to be used when a database connection is not available
notConnectedModelContext :: Logger -> ModelContext
notConnectedModelContext logger = ModelContext
    { connectionPool = error "Not connected"
    , transactionConnection = Nothing
    , logger = logger
    , trackTableReadCallback = Nothing
    , rowLevelSecurity = Nothing
    }

createModelContext :: NominalDiffTime -> Int -> ByteString -> Logger -> IO ModelContext
createModelContext idleTime maxConnections databaseUrl logger = do
    numStripes <- GHC.Conc.getNumCapabilities
    let create = PG.connectPostgreSQL databaseUrl
    let destroy = PG.close
    connectionPool <- Pool.createPool create destroy numStripes idleTime maxConnections

    let trackTableReadCallback = Nothing
    let transactionConnection = Nothing
    let rowLevelSecurity = Nothing
    pure ModelContext { .. }

instance LoggingProvider ModelContext where
    getLogger ModelContext { .. } = logger

type family GetModelById id :: Type where
    GetModelById (Maybe (Id' tableName)) = Maybe (GetModelByTableName tableName)
    GetModelById (Id' tableName) = GetModelByTableName tableName
type family GetTableName model :: Symbol
type family GetModelByTableName (tableName :: Symbol) :: Type

class CanCreate a where
    create :: (?modelContext :: ModelContext) => a -> IO a
    createMany :: (?modelContext :: ModelContext) => [a] -> IO [a]

class CanUpdate a where
    updateRecord :: (?modelContext :: ModelContext) => a -> IO a

{-# INLINE createRecord #-}
createRecord :: (?modelContext :: ModelContext, CanCreate model) => model -> IO model
createRecord = create

class InputValue a where
    inputValue :: a -> Text

instance InputValue Text where
    inputValue text = text

instance InputValue Int where
    inputValue = tshow

instance InputValue Integer where
    inputValue = tshow

instance InputValue Double where
    inputValue double = cs (Numeric.showFFloat Nothing double "")

instance InputValue Float where
    inputValue float = cs (Numeric.showFFloat Nothing float "")

instance InputValue Bool where
    inputValue True = "on"
    inputValue False = "off"

instance InputValue Data.UUID.UUID where
    inputValue = Data.UUID.toText

instance InputValue () where
    inputValue () = "error: inputValue(()) not supported"

instance InputValue UTCTime where
    inputValue time = cs (iso8601Show time)

instance InputValue LocalTime where
    inputValue time = cs (iso8601Show time)

instance InputValue Day where
    inputValue date = cs (iso8601Show date)

instance InputValue TimeOfDay where
    inputValue timeOfDay = tshow timeOfDay

instance InputValue fieldType => InputValue (Maybe fieldType) where
    inputValue (Just value) = inputValue value
    inputValue Nothing = ""

instance InputValue value => InputValue [value] where
    inputValue list = list |> map inputValue |> intercalate ","

instance InputValue Aeson.Value where
    inputValue json = json |> Aeson.encode |> cs

instance InputValue Scientific where
    inputValue = tshow

instance Default Text where
    {-# INLINE def #-}
    def = ""

instance Default Bool where
    {-# INLINE def #-}
    def = False

instance Default Point where
    def = Point def def

instance Default Polygon where
    def = Polygon [def]

instance Default TSVector where
    def = TSVector def

instance Default Scientific where
    def = 0

type FieldName = ByteString

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
isNew model = model
        |> get #meta
        |> get #originalDatabaseRecord
        |> isNothing
{-# INLINABLE isNew #-}

type family GetModelName model :: Symbol

-- | Provides the primary key type for a given table. The instances are usually declared
-- by the generated haskell code in Generated.Types
--
-- __Example:__ Defining the primary key for a users table
--
-- > type instance PrimaryKey "users" = UUID
--
--
-- __Example:__ Defining the primary key for a table with a SERIAL pk
--
-- > type instance PrimaryKey "projects" = Int
--
type family PrimaryKey (tableName :: Symbol)

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

newtype Id' table = Id (PrimaryKey table)

deriving instance (Eq (PrimaryKey table)) => Eq (Id' table)
deriving instance (Ord (PrimaryKey table)) => Ord (Id' table)
deriving instance (Hashable (PrimaryKey table)) => Hashable (Id' table)
deriving instance (KnownSymbol table, Data (PrimaryKey table)) => Data (Id' table)
deriving instance (KnownSymbol table, NFData (PrimaryKey table)) => NFData (Id' table)

-- | We need to map the model to its table name to prevent infinite recursion in the model data definition
-- E.g. `type Project = Project' { id :: Id Project }` will not work
-- But `type Project = Project' { id :: Id "projects" }` will
type Id model = Id' (GetTableName model)

instance InputValue (PrimaryKey model') => InputValue (Id' model') where
    {-# INLINE inputValue #-}
    inputValue = inputValue . unpackId

instance IsEmpty (PrimaryKey table) => IsEmpty (Id' table) where
    isEmpty (Id primaryKey) = isEmpty primaryKey

recordToInputValue :: (HasField "id" entity (Id entity), Show (PrimaryKey (GetTableName entity))) => entity -> Text
recordToInputValue entity =
    getField @"id" entity
    |> unpackId
    |> tshow
{-# INLINE recordToInputValue #-}

instance FromField (PrimaryKey model) => FromField (Id' model) where
    {-# INLINE fromField #-}
    fromField value metaData = do
        fieldValue <- fromField value metaData
        pure (Id fieldValue)

instance ToField (PrimaryKey model) => ToField (Id' model) where
    {-# INLINE toField #-}
    toField = toField . unpackId

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

-- | Record type for objects of model types labeled with values from different database tables. (e.g. comments labeled with the IDs of the posts they belong to).
data LabeledData a b = LabeledData { labelValue :: a, contentValue :: b }
    deriving (Show)

instance (FromField label, PG.FromRow a) => PGFR.FromRow (LabeledData label a) where
    fromRow = LabeledData <$> PGFR.field <*> PGFR.fromRow

-- | Sometimes you have a hardcoded UUID value which represents some record id. This instance allows you
-- to write the Id like a string:
--
-- > let projectId = "ca63aace-af4b-4e6c-bcfa-76ca061dbdc6" :: Id Project
instance (Read (PrimaryKey model), ParsePrimaryKey (PrimaryKey model)) => IsString (Id' model) where
    fromString uuid = textToId uuid
    {-# INLINE fromString #-}

class ParsePrimaryKey primaryKey where
    parsePrimaryKey :: Text -> Maybe primaryKey

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
measureTimeIfLogging :: (?modelContext :: ModelContext, Show q) => IO a -> Query -> q -> IO a
measureTimeIfLogging queryAction theQuery theParameters = do
    let currentLogLevel = get #logger ?modelContext |> get #level
    if currentLogLevel == Debug
        then do
            start <- getCurrentTime
            queryAction `finally` do
                end <- getCurrentTime
                let theTime = end `diffUTCTime` start
                logQuery theQuery theParameters theTime
        else queryAction

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
sqlQuery :: (?modelContext :: ModelContext, PG.ToRow q, PG.FromRow r, Show q) => Query -> q -> IO [r]
sqlQuery theQuery theParameters = do
    measureTimeIfLogging
        (withDatabaseConnection \connection -> enhanceSqlError theQuery theParameters do
            withRLSParams (PG.query connection) theQuery theParameters
        )
        theQuery
        theParameters
{-# INLINABLE sqlQuery #-}

-- | Runs a sql statement (like a CREATE statement)
--
-- __Example:__
--
-- > sqlExec "CREATE TABLE users ()" ()
sqlExec :: (?modelContext :: ModelContext, PG.ToRow q, Show q) => Query -> q -> IO Int64
sqlExec theQuery theParameters = do
    measureTimeIfLogging
        (withDatabaseConnection \connection -> enhanceSqlError theQuery theParameters do
            withRLSParams (PG.execute connection) theQuery theParameters
        )
        theQuery
        theParameters
{-# INLINABLE sqlExec #-}

-- | Wraps the query with Row level security boilerplate, if a row level security context was provided
--
-- __Example:__
--
-- If a row level security context is given, this will turn a query like the following
--
-- > withRLSParams runQuery "SELECT * FROM projects WHERE id = ?" (Only "..")
--
-- Into the following equivalent:
--
-- > runQuery "SET LOCAL ROLE ?; SET LOCAL rls.ihp_user_id = ?; SELECT * FROM projects WHERE id = ?" ["ihp_authenticated", "<user id>", .."]
--
withRLSParams :: (?modelContext :: ModelContext, PG.ToRow params) => (PG.Query -> [PG.Action] -> result) -> PG.Query -> params -> result
withRLSParams runQuery query params = do
    case get #rowLevelSecurity ?modelContext of
        Just RowLevelSecurityContext { rlsAuthenticatedRole, rlsUserId } -> do
            let query' = "SET LOCAL ROLE ?; SET LOCAL rls.ihp_user_id = ?; " <> query
            let params' = [PG.toField (PG.Identifier rlsAuthenticatedRole), PG.toField rlsUserId] <> PG.toRow params
            runQuery query' params'
        Nothing -> runQuery query (PG.toRow params)

withDatabaseConnection :: (?modelContext :: ModelContext) => (Connection -> IO a) -> IO a
withDatabaseConnection block =
    let
        ModelContext { connectionPool, transactionConnection, rowLevelSecurity } = ?modelContext
    in case transactionConnection of
        Just transactionConnection -> block transactionConnection
        Nothing -> Pool.withResource connectionPool block
{-# INLINABLE withDatabaseConnection #-}

-- | Runs a raw sql query which results in a single scalar value such as an integer or string
--
-- __Example:__
--
-- > usersCount <- sqlQueryScalar "SELECT COUNT(*) FROM users"
--
-- Take a look at "IHP.QueryBuilder" for a typesafe approach on building simple queries.
sqlQueryScalar :: (?modelContext :: ModelContext) => (PG.ToRow q, Show q, FromField value) => Query -> q -> IO value
sqlQueryScalar theQuery theParameters = do
    result <- measureTimeIfLogging
        (withDatabaseConnection \connection -> enhanceSqlError theQuery theParameters do
            PG.query connection theQuery theParameters
        )
        theQuery
        theParameters
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
sqlQueryScalarOrNothing :: (?modelContext :: ModelContext) => (PG.ToRow q, Show q, FromField value) => Query -> q -> IO (Maybe value)
sqlQueryScalarOrNothing theQuery theParameters = do
    result <- measureTimeIfLogging
        (withDatabaseConnection \connection -> enhanceSqlError theQuery theParameters do
            PG.query connection theQuery theParameters
        )
        theQuery
        theParameters
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
-- >        |> set #companyId (get #id company)
-- >        |> createRecord
-- >
-- >    company <- company
-- >        |> set #ownerId (get #id user)
-- >        |> updateRecord
withTransaction :: (?modelContext :: ModelContext) => ((?modelContext :: ModelContext) => IO a) -> IO a
withTransaction block = withTransactionConnection do
    let connection = ?modelContext
            |> get #transactionConnection
            |> \case
                Just connection -> connection
                Nothing -> error "withTransaction: transactionConnection not set as expected"
    PG.withTransaction connection block
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

-- | Returns the postgres connection when called within a 'withTransaction' block
--
-- Throws an error if called from outside a 'withTransaction'
transactionConnectionOrError :: (?modelContext :: ModelContext) => Connection
transactionConnectionOrError = ?modelContext
            |> get #transactionConnection
            |> \case
                Just connection -> connection
                Nothing -> error "getTransactionConnectionOrError: Not in a transaction state"

commitTransaction :: (?modelContext :: ModelContext) => IO ()
commitTransaction = PG.commit transactionConnectionOrError
{-# INLINABLE commitTransaction #-}

rollbackTransaction :: (?modelContext :: ModelContext) => IO ()
rollbackTransaction = PG.rollback transactionConnectionOrError
{-# INLINABLE rollbackTransaction #-}

withTransactionConnection :: (?modelContext :: ModelContext) => ((?modelContext :: ModelContext) => IO a) -> IO a
withTransactionConnection block = do
    let ModelContext { connectionPool } = ?modelContext
    Pool.withResource connectionPool \connection -> do
        let modelContext = ?modelContext { transactionConnection = Just connection }
        let ?modelContext = modelContext in block
{-# INLINABLE withTransactionConnection #-}

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
    tableNameByteString = Text.encodeUtf8 (tableName @record)
    {-# INLINE tableNameByteString #-}

    -- | Returns the list of column names for a given model
    --
    -- __Example:__
    --
    -- >>> columnNames @User
    -- ["id", "email", "created_at"]
    --
    columnNames :: [ByteString]

    -- | Returns WHERE conditions to match an entity by it's primary key
    --
    -- For tables with a simple primary key this returns a tuple with the id:
    --
    -- >>> primaryKeyCondition project
    -- [("id", "d619f3cf-f355-4614-8a4c-e9ea4f301e39")]
    --
    -- If the table has a composite primary key, this returns multiple elements:
    --
    -- >>> primaryKeyCondition postTag
    -- [("post_id", "0ace9270-568f-4188-b237-3789aa520588"), ("tag_id", "0b58fdf5-4bbb-4e57-a5b7-aa1c57148e1c")]
    --
    primaryKeyCondition :: record -> [(Text, PG.Action)]
    default primaryKeyCondition :: forall id. (HasField "id" record id, ToField id) => record -> [(Text, PG.Action)]
    primaryKeyCondition record = [("id", toField (get #id record))]

logQuery :: (?modelContext :: ModelContext, Show query, Show parameters) => query -> parameters -> NominalDiffTime -> IO ()
logQuery query parameters time = do
        let ?context = ?modelContext
        -- NominalTimeDiff is represented as seconds, and doesn't provide a FormatTime option for printing in ms.
        -- To get around that we convert to and from a rational so we can format as desired.
        let queryTimeInMs = (time * 1000) |> toRational |> fromRational @Double
        let formatRLSInfo userId = " { ihp_user_id = " <> userId <> " }"
        let rlsInfo = case get #rowLevelSecurity ?context of
                Just RowLevelSecurityContext { rlsUserId = PG.Plain rlsUserId } -> formatRLSInfo (cs (Builder.toLazyByteString rlsUserId))
                Just RowLevelSecurityContext { rlsUserId = rlsUserId } -> formatRLSInfo (tshow rlsUserId)
                Nothing -> ""
        Log.debug ("Query (" <>  tshow queryTimeInMs <> "ms): " <> tshow query <> " " <> tshow parameters <> rlsInfo)
{-# INLINABLE logQuery #-}

-- | Runs a @DELETE@ query for a record.
--
-- >>> let project :: Project = ...
-- >>> deleteRecord project
-- DELETE FROM projects WHERE id = '..'
--
-- Use 'deleteRecords' if you want to delete multiple records.
deleteRecord :: forall record table. (?modelContext :: ModelContext, Show (PrimaryKey table), Table record, HasField "id" record (Id' table), ToField (PrimaryKey table), GetModelByTableName table ~ record, Show (PrimaryKey table), ToField (PrimaryKey table)) => record -> IO ()
deleteRecord record =
    deleteRecordById @record (get #id record)
{-# INLINABLE deleteRecord #-}

-- | Like 'deleteRecord' but using an Id
--
-- >>> let project :: Id Project = ...
-- >>> delete projectId
-- DELETE FROM projects WHERE id = '..'
--
deleteRecordById :: forall record table. (?modelContext :: ModelContext, Table record, ToField (PrimaryKey table), Show (PrimaryKey table), record ~ GetModelByTableName table) => Id' table -> IO ()
deleteRecordById id = do
    let theQuery = "DELETE FROM " <> tableName @record <> " WHERE id = ?"
    let theParameters = PG.Only id
    sqlExec (PG.Query . cs $! theQuery) theParameters
    pure ()
{-# INLINABLE deleteRecordById #-}

-- | Runs a @DELETE@ query for a list of records.
--
-- >>> let projects :: [Project] = ...
-- >>> deleteRecords projects
-- DELETE FROM projects WHERE id IN (..)
deleteRecords :: forall record table. (?modelContext :: ModelContext, Show (PrimaryKey table), Table record, HasField "id" record (Id' table), ToField (PrimaryKey table), record ~ GetModelByTableName table) => [record] -> IO ()
deleteRecords records =
    deleteRecordByIds @record (ids records)
{-# INLINABLE deleteRecords #-}

-- | Like 'deleteRecordById' but for a list of Ids.
--
-- >>> let projectIds :: [ Id Project ] = ...
-- >>> delete projectIds
-- DELETE FROM projects WHERE id IN ('..')
--
deleteRecordByIds :: forall record table. (?modelContext :: ModelContext, Show (PrimaryKey table), Table record, ToField (PrimaryKey table), record ~ GetModelByTableName table) => [Id' table] -> IO ()
deleteRecordByIds ids = do
    let theQuery = "DELETE FROM " <> tableName @record <> " WHERE id IN ?"
    let theParameters = (PG.Only (PG.In ids))
    sqlExec (PG.Query . cs $! theQuery) theParameters
    pure ()
{-# INLINABLE deleteRecordByIds #-}

-- | Runs a @DELETE@ query to delete all rows in a table.
--
-- >>> deleteAll @Project
-- DELETE FROM projects
deleteAll :: forall record. (?modelContext :: ModelContext, Table record) => IO ()
deleteAll = do
    let theQuery = "DELETE FROM " <> tableName @record
    sqlExec (PG.Query . cs $! theQuery) ()
    pure ()
{-# INLINABLE deleteAll #-}

type family Include (name :: GHC.Types.Symbol) model

type family Include' (name :: [GHC.Types.Symbol]) model where
    Include' '[] model = model
    Include' (x:xs) model = Include' xs (Include x model)

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

class Record model where
    newRecord :: model

-- | Helper type to deal with models where relations are included or that are only partially fetched
-- Examples:
--
-- >>> NormalizeModel (Include "author_id" Post)
-- Post
--
-- >>> NormalizeModel Post
-- Post
type NormalizeModel model = GetModelByTableName (GetTableName model)

-- | Returns the ids for a list of models
--
-- Shorthand for @map (get #id) records@.
--
-- >>> users <- query @User |> fetch
-- >>> ids users
-- [227fbba3-0578-4eb8-807d-b9b692c3644f, 9d7874f2-5343-429b-bcc4-8ee62a5a6895, ...] :: [Id User]
ids :: (HasField "id" record id) => [record] -> [id]
ids records = map (getField @"id") records
{-# INLINE ids #-}

-- | The error message of a validator can be either a plain text value or a HTML formatted value
data Violation
    = TextViolation { message :: !Text } -- ^ Plain text validation error, like "cannot be empty"
    | HtmlViolation { message :: !Text } -- ^ HTML formatted, already pre-escaped validation error, like "Invalid, please <a href="http://example.com">check the documentation</a>"
    deriving (Eq, Show)

-- | Every IHP database record has a magic @meta@ field which keeps a @MetaBag@ inside. This data structure is used e.g. to keep track of the validation errors that happend.
data MetaBag = MetaBag
    { annotations            :: ![(Text, Violation)] -- ^ Stores validation failures, as a list of (field name, error) pairs. E.g. @annotations = [ ("name", TextViolation "cannot be empty") ]@
    , touchedFields          :: ![Text] -- ^ Whenever a 'set' is callled on a field, it will be marked as touched. Only touched fields are saved to the database when you call 'updateRecord'
    , originalDatabaseRecord :: Maybe Dynamic -- ^ When the record has been fetched from the database, we save the initial database record here. This is used by 'didChange' to check if a field value is different from the initial database value.
    } deriving (Show)

instance Eq MetaBag where
    MetaBag { annotations, touchedFields } == MetaBag { annotations = annotations', touchedFields = touchedFields' } = annotations == annotations' && touchedFields == touchedFields'

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
didChangeRecord record =
    record
    |> get #meta
    |> get #touchedFields
    |> isEmpty

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
didChange field record = didTouchField && didChangeField
    where
        didTouchField :: Bool
        didTouchField =
            record
            |> get #meta
            |> get #touchedFields
            |> includes (cs $! symbolVal field)

        didChangeField :: Bool
        didChangeField = originalFieldValue /= fieldValue

        fieldValue :: fieldValue
        fieldValue = record |> getField @fieldName

        originalFieldValue :: fieldValue
        originalFieldValue =
            record
            |> get #meta
            |> get #originalDatabaseRecord
            |> fromMaybe (error "didChange called on a record without originalDatabaseRecord")
            |> fromDynamic @record
            |> fromMaybe (error "didChange failed to retrieve originalDatabaseRecord")
            |> getField @fieldName

-- | Represents fields that have a default value in an SQL schema
--
--   The 'Default' constructor represents the default value from the schema,
--   while the 'NonDefault' constructor holds some other value for the field
data FieldWithDefault valueType = Default | NonDefault valueType deriving (Eq, Show)

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
  | cs (symbolVal name) `elem` get #touchedFields (get #meta model) =
    NonDefault (get name model)
  | otherwise = Default

-- | Represents fields that may have been updated
--
--   The 'NoUpdate' constructor represents the existing value in the database,
--   while the 'Update' constructor holds some new value for the field
data FieldWithUpdate name value
  = NoUpdate (Proxy name)
  | Update value
  deriving (Eq, Show)

instance (KnownSymbol name, ToField value) => ToField (FieldWithUpdate name value) where
  toField (NoUpdate name) =
    Plain (ClassyPrelude.fromString $ cs $ fieldNameToColumnName $ cs $ symbolVal name)
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
  | cs (symbolVal name) `elem` get #touchedFields (get #meta model) =
    Update (get name model)
  | otherwise = NoUpdate name

instance (ToJSON (PrimaryKey a)) => ToJSON (Id' a) where
  toJSON (Id a) = toJSON a

instance (FromJSON (PrimaryKey a)) => FromJSON (Id' a) where
    parseJSON value = Id <$> parseJSON value

-- | Thrown by 'fetchOne' when the query result is empty
data RecordNotFoundException
    = RecordNotFoundException { queryAndParams :: (ByteString, [Action]) }
    deriving (Show)

instance Exception RecordNotFoundException

-- | Whenever calls to 'Database.PostgreSQL.Simple.query' or 'Database.PostgreSQL.Simple.execute'
-- raise an 'Database.PostgreSQL.Simple.SqlError' exception, we wrap that exception in this data structure.
-- This allows us to show the actual database query that has triggered the error.
data EnhancedSqlError
    = EnhancedSqlError
    { sqlErrorQuery :: Query
    , sqlErrorQueryParams :: [Action]
    , sqlError :: PG.SqlError
    } deriving (Show)

-- | Catches 'SqlError' and wraps them in 'EnhancedSqlError'
enhanceSqlError :: PG.ToRow parameters => Query -> parameters -> IO a -> IO a
enhanceSqlError sqlErrorQuery sqlErrorQueryParams block = catch block (\sqlError -> throwIO EnhancedSqlError { sqlErrorQuery, sqlErrorQueryParams = PG.toRow sqlErrorQueryParams, sqlError })
{-# INLINE enhanceSqlError #-}

instance Exception EnhancedSqlError

instance Default Aeson.Value where
    def = Aeson.Null


-- | This instancs allows us to avoid wrapping lists with PGArray when
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
trackTableRead :: (?modelContext :: ModelContext) => ByteString -> IO ()
trackTableRead tableName = case get #trackTableReadCallback ?modelContext of
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
withTableReadTracker :: (?modelContext :: ModelContext) => ((?modelContext :: ModelContext, ?touchedTables :: IORef (Set ByteString)) => IO ()) -> IO ()
withTableReadTracker trackedSection = do
    touchedTablesVar <- newIORef Set.empty
    let trackTableReadCallback = Just \tableName -> modifyIORef' touchedTablesVar (Set.insert tableName)
    let oldModelContext = ?modelContext
    let ?modelContext = oldModelContext { trackTableReadCallback }
    let ?touchedTables = touchedTablesVar
    trackedSection
