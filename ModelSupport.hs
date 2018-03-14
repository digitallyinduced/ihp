{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts, AllowAmbiguousTypes #-}

module Foundation.ModelSupport where

import Foundation.HaskellSupport
import ClassyPrelude
import Database.PostgreSQL.Simple (Connection)
import qualified Text.Inflections
import Database.PostgreSQL.Simple.Types (Query (Query))
import Data.Default

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

instance InputValue () where
    inputValue () = "error: inputValue(()) not supported"

data QueryCondition a = NoCondition | Equal a

type FieldName = ByteString
toSQLCondition :: FieldName -> QueryCondition a -> (ByteString, Maybe a)
toSQLCondition _ NoCondition = ("? IS NULL", Nothing)
toSQLCondition fieldName (Equal a) = (fieldName <> " = ?", Just a)

class IsNew model where
    isNew :: model -> Bool

class HasModelName model where
    getModelName :: model -> Text