{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts, AllowAmbiguousTypes, UndecidableInstances, FlexibleInstances, IncoherentInstances, DataKinds, PolyKinds, TypeApplications, ScopedTypeVariables, TypeInType, ConstraintKinds, TypeOperators, GADTs, GeneralizedNewtypeDeriving #-}

module IHP.ModelSupport
( module IHP.ModelSupport
, module IHP.Postgres.Point
, module IHP.Postgres.Inet
) where

import IHP.HaskellSupport
import IHP.NameSupport
import qualified Prelude
import ClassyPrelude hiding (UTCTime, find, ModifiedJulianDay)
import qualified ClassyPrelude
import Database.PostgreSQL.Simple (Connection)
import qualified Text.Inflections
import Database.PostgreSQL.Simple.Types (Query (Query))
import Database.PostgreSQL.Simple.FromField hiding (Field, name)
import Database.PostgreSQL.Simple.ToField
import Data.Default
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.String.Conversions (cs ,ConvertibleStrings)
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Calendar
import Unsafe.Coerce
import Data.UUID
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Types as PG
import GHC.Records
import GHC.OverloadedLabels
import GHC.TypeLits
import GHC.Types
import Data.Proxy
import Data.Data
import qualified Control.Newtype.Generics as Newtype
import Control.Applicative (Const)
import qualified GHC.Types as Type
import qualified Data.Text as Text
import Data.Aeson (ToJSON (..))
import qualified Data.Aeson as Aeson
import qualified Data.Set as Set
import qualified Text.Read as Read
import qualified Data.Pool as Pool
import qualified GHC.Conc
import IHP.Postgres.Point
import IHP.Postgres.Inet
import qualified Data.ByteString.Char8 as ByteString
import IHP.Log.Types
import qualified IHP.Log.Logging as Log

-- | Provides the db connection and some IHP-specific db configuration
data ModelContext = ModelContext
    { connectionPool :: Pool.Pool Connection
    -- | If True, prints out all SQL queries that are executed. Will be set to True by default in development mode (as configured in Config.hs) and False in production.
    , logger :: Logger
    -- | A callback that is called whenever a specific table is accessed using a SELECT query
    , trackTableReadCallback :: Maybe (ByteString -> IO ())
    }

-- | Provides a mock ModelContext to be used when a database connection is not available
notConnectedModelContext :: Logger -> ModelContext
notConnectedModelContext logger = ModelContext
    { connectionPool = error "Not connected"
    , logger = logger
    , trackTableReadCallback = Nothing
    }

createModelContext :: NominalDiffTime -> Int -> ByteString -> Logger -> IO ModelContext
createModelContext idleTime maxConnections databaseUrl logger = do
    numStripes <- GHC.Conc.getNumCapabilities
    let create = PG.connectPostgreSQL databaseUrl
    let destroy = PG.close
    connectionPool <- Pool.createPool create destroy numStripes idleTime maxConnections

    let queryDebuggingEnabled = False -- The app server will override this in dev mode and set it to True
    let trackTableReadCallback = Nothing
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
    inputValue = tshow

instance InputValue Float where
    inputValue = tshow

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

instance InputValue fieldType => InputValue (Maybe fieldType) where
    inputValue (Just value) = inputValue value
    inputValue Nothing = ""

instance InputValue value => InputValue [value] where
    inputValue list = list |> map inputValue |> intercalate ","

instance InputValue Aeson.Value where
    inputValue json = json |> Aeson.encode |> cs

instance Default Text where
    {-# INLINE def #-}
    def = ""

instance Default Bool where
    {-# INLINE def #-}
    def = False

instance Default Point where
    def = Point def def

type FieldName = ByteString

-- | Returns @True@ when the record has not been saved to the database yet. Returns @False@ otherwise.
--
-- __Example:__ Returns @False@ when a record has not been inserted yet.
--
-- >>> let project = newRecord @Project
-- >>> isNew project
-- False
--
-- __Example:__ Returns @True@ after inserting a record.
--
-- >>> project <- createRecord project
-- >>> isNew project
-- True
--
-- __Example:__ Returns @True@ for records which have been fetched from the database.
--
-- >>> book <- query @Book |> fetchOne
-- >>> isNew book
-- False
isNew :: forall model id. (HasField "id" model id, Default id, Eq id) => model -> Bool
isNew model = def == (getField @"id" model)
{-# INLINE isNew #-}

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

-- | We need to map the model to it's table name to prevent infinite recursion in the model data definition
-- E.g. `type Project = Project' { id :: Id Project }` will not work
-- But `type Project = Project' { id :: Id "projects" }` will
type Id model = Id' (GetTableName model)

instance InputValue (PrimaryKey model') => InputValue (Id' model') where
    {-# INLINE inputValue #-}
    inputValue = inputValue . Newtype.unpack

instance IsEmpty (PrimaryKey table) => IsEmpty (Id' table) where
    isEmpty (Id primaryKey) = isEmpty primaryKey

recordToInputValue :: (HasField "id" entity (Id entity), Show (PrimaryKey (GetTableName entity))) => entity -> Text
recordToInputValue entity =
    getField @"id" entity
    |> Newtype.unpack
    |> tshow
{-# INLINE recordToInputValue #-}

instance FromField (PrimaryKey model) => FromField (Id' model) where
    {-# INLINE fromField #-}
    fromField value metaData = do
        fieldValue <- fromField value metaData
        pure (Id fieldValue)

instance ToField (PrimaryKey model) => ToField (Id' model) where
    {-# INLINE toField #-}
    toField = toField . Newtype.unpack

instance Show (PrimaryKey model) => Show (Id' model) where
    {-# INLINE show #-}
    show = show . Newtype.unpack

instance Newtype.Newtype (Id' model) where
    type O (Id' model) = PrimaryKey model
    pack = Id
    unpack (Id uuid) = uuid

-- | Sometimes you have a hardcoded UUID value which represents some record id. This instance allows you
-- to write the Id like a string:
--
-- > let projectId = "ca63aace-af4b-4e6c-bcfa-76ca061dbdc6" :: Id Project
instance (Read (PrimaryKey model), ParsePrimaryKey (PrimaryKey model)) => IsString (Id' model) where
    fromString uuid = textToId uuid

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
textToId :: (ParsePrimaryKey (PrimaryKey model), ConvertibleStrings text Text) => text -> Id' model
textToId text = case parsePrimaryKey (cs text) of
        Just id -> Id id
        Nothing -> error (cs $ "Unable to convert " <> (cs text :: Text) <> " to Id value. Is it a valid uuid?")
{-# INLINE textToId #-}

instance Default (PrimaryKey model) => Default (Id' model) where
    {-# INLINE def #-}
    def = Newtype.pack def

-- | Runs a raw sql query
--
-- __Example:__
--
-- > users <- sqlQuery "SELECT id, firstname, lastname FROM users" ()
--
-- Take a look at "IHP.QueryBuilder" for a typesafe approach on building simple queries.
sqlQuery :: (?modelContext :: ModelContext, PG.ToRow q, PG.FromRow r, Show q) => Query -> q -> IO [r]
sqlQuery theQuery theParameters = do
    logQuery theQuery theParameters
    withDatabaseConnection \connection -> PG.query connection theQuery theParameters
{-# INLINABLE sqlQuery #-}


-- | Runs a sql statement (like a CREATE statement)
--
-- __Example:__
--
-- > sqlExec "CREATE TABLE users ()" ()
sqlExec :: (?modelContext :: ModelContext, PG.ToRow q, Show q) => Query -> q -> IO Int64
sqlExec theQuery theParameters = do
    logQuery theQuery theParameters
    withDatabaseConnection \connection -> PG.execute connection theQuery theParameters
{-# INLINABLE sqlExec #-}

withDatabaseConnection :: (?modelContext :: ModelContext) => (Connection -> IO a) -> IO a
withDatabaseConnection block = let ModelContext { connectionPool } = ?modelContext in Pool.withResource connectionPool block
{-# INLINABLE withDatabaseConnection #-}

-- | Runs a raw sql query which results in a single scalar value such as an integer or string
--
-- __Example:__
--
-- > usersCount <- sqlQuery "SELECT COUNT(*) FROM users"
--
-- Take a look at "IHP.QueryBuilder" for a typesafe approach on building simple queries.
sqlQueryScalar :: (?modelContext :: ModelContext) => (PG.ToRow q, FromField value) => Query -> q -> IO value
sqlQueryScalar query parameters = do
    result <- withDatabaseConnection \connection -> PG.query connection query parameters
    pure case result of
        [PG.Only result] -> result
        _ -> error "sqlQueryScalar: Expected a scalar result value"
{-# INLINABLE sqlQueryScalar #-}

-- | Returns the table name of a given model.
--
-- __Example:__
--
-- >>> tableName @User
-- "users"
--
tableName :: forall model. (KnownSymbol (GetTableName model)) => Text
tableName = symbolToText @(GetTableName model)
{-# INLINE tableName #-}

-- | Returns the table name of a given model as a bytestring.
--
-- __Example:__
--
-- >>> tableNameByteString @User
-- "users"
--
tableNameByteString :: forall model. (KnownSymbol (GetTableName model)) => ByteString
tableNameByteString = symbolToByteString @(GetTableName model)
{-# INLINE tableNameByteString #-}

logQuery :: (?modelContext :: ModelContext, Show query, Show parameters) => query -> parameters -> IO ()
logQuery query parameters = do
        let logMessage = (query, parameters)
                |> tshow
        let ?context = ?modelContext
        Log.info logMessage
{-# INLINABLE logQuery #-}

-- | Runs a @DELETE@ query for a record.
--
-- >>> let project :: Project = ...
-- >>> deleteRecord project
-- DELETE FROM projects WHERE id = '..'
--
-- Use 'deleteRecords' if you want to delete multiple records.
deleteRecord :: forall model id. (?modelContext :: ModelContext, Show id, KnownSymbol (GetTableName model), HasField "id" model id, ToField id) => model -> IO ()
deleteRecord model = get #id model |> deleteRecordById @model @id
{-# INLINABLE deleteRecord #-}

-- | Like 'deleteRecord' but using an Id
--
-- >>> let project :: Id Project = ...
-- >>> delete projectId
-- DELETE FROM projects WHERE id = '..'
--
deleteRecordById :: forall model id. (?modelContext :: ModelContext, Show id, KnownSymbol (GetTableName model), HasField "id" model id, ToField id) => id -> IO ()
deleteRecordById id = do
    let theQuery = "DELETE FROM " <> tableName @model <> " WHERE id = ?"
    let theParameters = (PG.Only id)
    logQuery theQuery theParameters
    sqlExec (PG.Query . cs $! theQuery) theParameters
    pure ()
{-# INLINABLE deleteRecordById #-}

-- | Runs a @DELETE@ query for a list of records.
--
-- >>> let projects :: [Project] = ...
-- >>> deleteRecords projects
-- DELETE FROM projects WHERE id IN (..)
deleteRecords :: forall record id. (?modelContext :: ModelContext, Show id, KnownSymbol (GetTableName record), HasField "id" record id, record ~ GetModelById id, ToField id) => [record] -> IO ()
deleteRecords records = do
    let theQuery = "DELETE FROM " <> tableName @record <> " WHERE id IN ?"
    let theParameters = PG.Only (PG.In (ids records))
    if length records > 10
        then logQuery theQuery "More than 10 records"
        else logQuery theQuery theParameters
    sqlExec (PG.Query . cs $! theQuery) theParameters
    pure ()
{-# INLINABLE deleteRecords #-}

-- | Runs a @DELETE@ query to delete all rows in a table.
--
-- >>> deleteAll @Project
-- DELETE FROM projects
deleteAll :: forall record. (?modelContext :: ModelContext, KnownSymbol (GetTableName record)) => IO ()
deleteAll = do
    let theQuery = "DELETE FROM " <> tableName @record
    logQuery theQuery ()
    sqlExec (PG.Query . cs $! theQuery) ()
    pure ()
{-# INLINABLE deleteAll #-}

type family Include (name :: GHC.Types.Symbol) model

type family Include' (name :: [GHC.Types.Symbol]) model where
    Include' '[] model = model
    Include' (x:xs) model = Include' xs (Include x model)

instance Default LocalTime where
    def = LocalTime def (TimeOfDay 0 0 0)

instance Default Day where
    def = ModifiedJulianDay 0

instance Default UTCTime where
    def = UTCTime def 0

instance Default (PG.Binary ByteString) where
    def = PG.Binary ""

instance Newtype.Newtype (PG.Binary payload) where
    type O (PG.Binary payload) = payload
    pack = PG.Binary
    unpack (PG.Binary payload) = payload

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

data MetaBag = MetaBag
  { annotations :: [(Text, Text)]
  , touchedFields :: [Text]
  } deriving (Eq, Show)

instance Default MetaBag where
    def = MetaBag { annotations = [], touchedFields = [] }
    {-# INLINE def #-}

instance SetField "annotations" MetaBag [(Text, Text)] where
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
didChange :: (KnownSymbol fieldName, HasField fieldName record fieldValue, HasField "meta" record MetaBag) => Proxy fieldName -> record -> Bool
didChange field record =
    record
    |> get #meta
    |> get #touchedFields
    |> includes (cs $! symbolVal field)

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


-- | Thrown by 'fetchOne' when the query result is empty
data RecordNotFoundException
    = RecordNotFoundException { queryAndParams :: (ByteString, [Action]) }
    deriving (Show)

instance Exception RecordNotFoundException

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
    let trackTableReadCallback = Just \tableName -> modifyIORef touchedTablesVar (Set.insert tableName)
    let oldModelContext = ?modelContext
    let ?modelContext = oldModelContext { trackTableReadCallback }
    let ?touchedTables = touchedTablesVar
    trackedSection
