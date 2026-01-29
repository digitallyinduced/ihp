{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts, AllowAmbiguousTypes, UndecidableInstances, FlexibleInstances, DataKinds, PolyKinds, TypeApplications, ScopedTypeVariables, ConstraintKinds, TypeOperators, GADTs, GeneralizedNewtypeDeriving #-}

{-|
Module: IHP.ModelSupport.Types
Description: Core types for IHP's model and database support
Copyright: (c) digitally induced GmbH, 2020

This module contains the core types for IHP's model support.
It's designed to be lightweight and avoid heavy dependencies,
allowing modules that only need the types to compile faster.

For the full model API including query functions, use 'IHP.ModelSupport'.
-}
module IHP.ModelSupport.Types
( -- * Model Context
  ModelContext (..)
, RowLevelSecurityContext (..)
  -- * Type Families
, GetModelById
, GetTableName
, GetModelByTableName
, PrimaryKey
, GetModelName
, Include
, Include'
, NormalizeModel
  -- * Id Types
, Id'(..)
, Id
  -- * Record Metadata
, MetaBag (..)
, Violation (..)
, FieldName
  -- * Field Wrappers
, FieldWithDefault (..)
, FieldWithUpdate (..)
  -- * Utility Types
, LabeledData (..)
  -- * Exceptions
, RecordNotFoundException (..)
, EnhancedSqlError (..)
, HasqlException (..)
  -- * Type Classes
, CanCreate (..)
, CanUpdate (..)
, ParsePrimaryKey (..)
, FromRow (..)
, FromField (..)
) where

import Prelude
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Hashable (Hashable)
import Control.DeepSeq (NFData)
import Control.Exception (Exception)
import qualified Hasql.Connection as Hasql
import qualified Hasql.Pool as Hasql
import qualified Hasql.Session as HasqlSession
import qualified Hasql.DynamicStatements.Snippet as Snippet
import qualified Hasql.Decoders as Decoders
import GHC.TypeLits
import GHC.Types
import Data.Data
import Data.Dynamic
import Data.Proxy
import IHP.Log.Types (Logger)

-- | Provides the db connection and some IHP-specific db configuration
data ModelContext = ModelContext
    { connectionPool :: Hasql.Pool -- ^ Used to get database connections when no 'transactionConnection' is set
    , transactionConnection :: Maybe Hasql.Connection -- ^ Set to a specific database connection when executing a database transaction
    -- | Logs all queries to this logger at log level info
    , logger :: Logger
    -- | A callback that is called whenever a specific table is accessed using a SELECT query
    , trackTableReadCallback :: Maybe (ByteString -> IO ())
    -- | Is set to a value if row level security was enabled at runtime
    , rowLevelSecurity :: Maybe RowLevelSecurityContext
    -- | The database URL used to create connections (needed for PGListener and transactions)
    , databaseUrl :: ByteString
    }

-- | When row level security is enabled at runtime, this keeps track of the current
-- logged in user and the postgresql role to switch to.
data RowLevelSecurityContext = RowLevelSecurityContext
    { rlsAuthenticatedRole :: Text -- ^ Default is @ihp_authenticated@. This value comes from the @IHP_RLS_AUTHENTICATED_ROLE@  env var.
    , rlsUserId :: Snippet.Snippet -- ^ The user id of the current logged in user, as a SQL snippet
    }

type family GetModelById id :: Type where
    GetModelById (Maybe (Id' tableName)) = Maybe (GetModelByTableName tableName)
    GetModelById (Id' tableName) = GetModelByTableName tableName

type family GetTableName model :: Symbol
type family GetModelByTableName (tableName :: Symbol) :: Type

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

type family GetModelName model :: Symbol

type family Include (name :: GHC.Types.Symbol) model

type family Include' (name :: [GHC.Types.Symbol]) model where
    Include' '[] model = model
    Include' (x:xs) model = Include' xs (Include x model)

-- | Helper type to deal with models where relations are included or that are only partially fetched
-- Examples:
--
-- >>> NormalizeModel (Include "author_id" Post)
-- Post
--
-- >>> NormalizeModel Post
-- Post
type NormalizeModel model = GetModelByTableName (GetTableName model)

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

type FieldName = ByteString

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

-- | Represents fields that have a default value in an SQL schema
--
--   The 'Default' constructor represents the default value from the schema,
--   while the 'NonDefault' constructor holds some other value for the field
data FieldWithDefault valueType = Default | NonDefault valueType deriving (Eq, Show)

-- | Represents fields that may have been updated
--
--   The 'NoUpdate' constructor represents the existing value in the database,
--   while the 'Update' constructor holds some new value for the field
data FieldWithUpdate name value
  = NoUpdate (Proxy name)
  | Update value
  deriving (Eq, Show)

-- | Record type for objects of model types labeled with values from different database tables. (e.g. comments labeled with the IDs of the posts they belong to).
data LabeledData a b = LabeledData { labelValue :: a, contentValue :: b }
    deriving (Show)

-- | Thrown by 'fetchOne' when the query result is empty
data RecordNotFoundException
    = RecordNotFoundException { queryAndParams :: ByteString }
    deriving (Show)

instance Exception RecordNotFoundException

-- | Whenever a database query raises an error, we wrap that exception in this data structure.
-- This allows us to show the actual database query that has triggered the error.
data EnhancedSqlError
    = EnhancedSqlError
    { sqlErrorQuery :: ByteString
    , sqlErrorQueryParams :: [ByteString]
    , sqlError :: Text
    } deriving (Show)

instance Exception EnhancedSqlError

-- | Thrown when a hasql session or pool query fails.
-- This preserves the structured error information from hasql instead of
-- converting it to a generic string via 'userError'.
data HasqlException
    = HasqlSessionError { sessionError :: HasqlSession.SessionError }
    | HasqlPoolError { poolError :: Hasql.UsageError }
    deriving (Show)

instance Exception HasqlException

class CanCreate a where
    create :: (?modelContext :: ModelContext) => a -> IO a
    createMany :: (?modelContext :: ModelContext) => [a] -> IO [a]

    -- | Like 'createRecord' but doesn't return the created record
    createRecordDiscardResult :: (?modelContext :: ModelContext) => a -> IO ()
    createRecordDiscardResult record = do
        _ <- create record
        pure ()

class CanUpdate a where
    updateRecord :: (?modelContext :: ModelContext) => a -> IO a

    -- | Like 'updateRecord' but doesn't return the updated record
    updateRecordDiscardResult :: (?modelContext :: ModelContext) => a -> IO ()
    updateRecordDiscardResult record = do
        _ <- updateRecord record
        pure ()

class ParsePrimaryKey primaryKey where
    parsePrimaryKey :: Text -> Maybe primaryKey

-- | Provides a hasql row decoder for a model type.
-- This replaces postgresql-simple's FromRow type class.
-- Instances are generated by the SchemaCompiler.
class FromRow a where
    fromRow :: Decoders.Row a

-- | Provides a hasql value decoder for a single column type.
-- This replaces postgresql-simple's FromField type class.
-- Used for decoding individual column values.
class FromField a where
    fromField :: Decoders.Value a
