{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts, AllowAmbiguousTypes, UndecidableInstances, FlexibleInstances, IncoherentInstances, DataKinds, PolyKinds, TypeApplications, ScopedTypeVariables, TypeInType, ConstraintKinds, TypeOperators, GADTs #-}

module Foundation.ModelSupport where

import Foundation.HaskellSupport
import ClassyPrelude hiding (UTCTime, find)
import qualified ClassyPrelude
import Database.PostgreSQL.Simple (Connection)
import qualified Text.Inflections
import Database.PostgreSQL.Simple.Types (Query (Query))
import Database.PostgreSQL.Simple.FromField hiding (Field, name)
import Database.PostgreSQL.Simple.ToField
import Data.Default
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.String.Conversions (cs)
import Data.Time.Clock (UTCTime)
import Unsafe.Coerce
import Data.UUID
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Types as PG
import GHC.Records
import GHC.OverloadedLabels
import Data.String.Conversions (cs)
import GHC.TypeLits
import GHC.Types
import Data.Proxy

data ModelContext = ModelContext Connection

type family GetModelById id :: Type
type family GetTableName model :: Symbol

class CanCreate a where
    type Created a :: Type
    create :: (?modelContext :: ModelContext) => a -> IO (Created a)

createRecord :: (?modelContext :: ModelContext, CanCreate model) => model -> IO (Created model)
createRecord = create

class FindWhere a where
    type FindWhereResult a :: Type
    findWhere :: (?modelContext :: ModelContext) => a -> IO [FindWhereResult a]
    buildCriteria :: a

class FormField field where
    formFieldName :: field -> Text
    formFieldLabel :: field -> Text
    formFieldLabel field =
        let
            name = formFieldName field
            (Right parts) = Text.Inflections.parseSnakeCase [] name
        in Text.Inflections.titleize parts

class FormFieldValue field model where
    formFieldValue :: field -> model -> Text

class InputValue a where
    inputValue :: a -> Text

instance InputValue Text where
    inputValue text = text

instance InputValue Int where
    inputValue = tshow

instance InputValue Bool where
    inputValue True = "yes"
    inputValue False = "no"

instance InputValue Data.UUID.UUID where
    inputValue = Data.UUID.toText

instance InputValue () where
    inputValue () = "error: inputValue(()) not supported"

instance InputValue UTCTime where
    inputValue time = cs (iso8601Show time)

instance InputValue ClassyPrelude.UTCTime where
    inputValue time = cs (iso8601Show ((unsafeCoerce time) :: UTCTime))

instance InputValue fieldType => InputValue (Maybe fieldType) where
    inputValue (Just value) = inputValue value
    inputValue Nothing = ""

instance Default Text where
    def = ""

data QueryCondition a = NoCondition | Equal a

type FieldName = ByteString
toSQLCondition :: FieldName -> QueryCondition a -> (ByteString, Maybe a)
toSQLCondition _ NoCondition = ("? IS NULL", Nothing)
toSQLCondition fieldName (Equal a) = (fieldName <> " = ?", Just a)

class IsNew model where
    isNew :: model -> Bool

class HasModelName model where
    getModelName :: model -> Text

class HasTableName model where
    getTableName :: model -> Text

class NewTypeWrappedUUID wrapperType where
    unwrap :: wrapperType -> UUID
    wrap :: UUID -> wrapperType

instance NewTypeWrappedUUID UUID where
    unwrap uuid = uuid
    wrap uuid = uuid

instance {-# OVERLAPPABLE #-} (NewTypeWrappedUUID wrapperType) => InputValue wrapperType where
    inputValue wrapped =
        let
            innerValue :: UUID
            innerValue = unwrap wrapped
        in
            (inputValue innerValue) :: Text

--instance {-# OVERLAPPABLE #-} (NewTypeWrappedUUID wrapperType) => FromField wrapperType where
--    fromField value metaData = do
--        fieldValue <- fromField value metaData
--        return $ ((wrap fieldValue) :: wrapperType)

--instance {-# OVERLAPPABLE #-} (NewTypeWrappedUUID wrapperType) => ToField wrapperType where
--    toField value =
--        let
--            value' :: UUID
--            value' = unwrap value
--        in toField value'

query :: (?modelContext :: ModelContext) => (PG.ToRow q, PG.FromRow r) => Query -> q -> IO [r]
query = let (ModelContext conn) = ?modelContext in PG.query conn

deleteRecord :: (?modelContext::ModelContext) => (HasTableName model, NewTypeWrappedUUID idType, HasField "id" model idType) => model -> IO ()
deleteRecord model = do
    let (ModelContext conn) = ?modelContext
    let id = getField @"id" model
    let tableName = getTableName model
    PG.execute conn (PG.Query . cs $ "DELETE FROM " <> tableName <> " WHERE id = ?") (PG.Only (unwrap id))
    return ()

findOrNothing :: forall id model. (?modelContext :: ModelContext) => (NewTypeWrappedUUID id, ToField id, PG.FromRow (GetModelById id), KnownSymbol (GetTableName (GetModelById id))) => id -> IO (Maybe (GetModelById id))
findOrNothing id = do
    let tableName = symbolVal @(GetTableName (GetModelById id)) Proxy
    results <- query (PG.Query $ "SELECT * FROM " <> cs tableName <> " WHERE id = ? LIMIT 1") [id]
    return $ headMay results

findModel :: forall id model. (?modelContext :: ModelContext) => (NewTypeWrappedUUID id, ToField id, PG.FromRow (GetModelById id), KnownSymbol (GetTableName (GetModelById id))) => id -> IO (GetModelById id)
findModel id = do
    result <- findOrNothing id
    return (fromMaybe (error "Model cannot be found") result)

findMany :: forall id model. (?modelContext :: ModelContext) => (NewTypeWrappedUUID id, ToField id, PG.FromRow (GetModelById id), KnownSymbol (GetTableName (GetModelById id))) => [id] -> IO [GetModelById id]
findMany ids = do
    let tableName = symbolVal @(GetTableName (GetModelById id)) Proxy
    query (PG.Query $ "SELECT * FROM " <> cs tableName <> " WHERE id IN ?") (PG.Only $ PG.In ids)

type family ModelFieldType model :: GHC.Types.*
type family ModelFieldValue model (field :: GHC.Types.Symbol) :: GHC.Types.Type
