{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts, AllowAmbiguousTypes, UndecidableInstances, FlexibleInstances, IncoherentInstances #-}

module Foundation.ModelSupport where

import Foundation.HaskellSupport
import ClassyPrelude hiding (UTCTime)
import qualified ClassyPrelude
import Database.PostgreSQL.Simple (Connection)
import qualified Text.Inflections
import qualified Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.Types (Query (Query))
import Database.PostgreSQL.Simple.FromField hiding (Field, name)
import Database.PostgreSQL.Simple.ToField
import Data.Default
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.String.Conversions (cs)
import Data.Time.Clock (UTCTime)
import Unsafe.Coerce
import Data.UUID

data ModelContext = ModelContext Connection

class CanCreate a where
    type Created a :: *
    create :: (?modelContext :: ModelContext) => a -> IO (Created a)

class FindWhere a where
    type FindWhereResult a :: *
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

data QueryCondition a = NoCondition | Equal a

type FieldName = ByteString
toSQLCondition :: FieldName -> QueryCondition a -> (ByteString, Maybe a)
toSQLCondition _ NoCondition = ("? IS NULL", Nothing)
toSQLCondition fieldName (Equal a) = (fieldName <> " = ?", Just a)

class IsNew model where
    isNew :: model -> Bool

class HasModelName model where
    getModelName :: model -> Text

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

instance {-# OVERLAPPABLE #-} (NewTypeWrappedUUID wrapperType) => FromField wrapperType where
    fromField value metaData = do
        fieldValue <- fromField value metaData
        return $ ((wrap fieldValue) :: wrapperType)

instance {-# OVERLAPPABLE #-} (NewTypeWrappedUUID wrapperType) => ToField wrapperType where
    toField value =
        let
            value' :: UUID
            value' = unwrap value
        in toField value'

query :: (?modelContext :: ModelContext) => (PG.ToRow q, PG.FromRow r) => Query -> q -> IO [r]
query = let (ModelContext conn) = ?modelContext in PG.query conn