{-# LANGUAGE PolyKinds, UndecidableInstances  #-}
module Foundation.SchemaTypes where

import ClassyPrelude hiding (length)
import Data.Maybe (fromJust)
import qualified Data.List as List
import GHC.Types

import Data.UUID
import Foundation.DatabaseSupport.Point

data Table = Table Text [Attribute]
           deriving (Show, Eq, Ord)

data Attribute' stringType = Field stringType (FieldType stringType)
               | BelongsTo stringType
               | HasMany { name :: stringType, inverseOf :: Maybe stringType }
               deriving (Eq, Ord)

instance (Show stringType) => Show (Attribute' stringType) where
  show (Field fieldName fieldType) = "Field " <> show fieldName <> " (" <> show fieldType <> ")"
  show (BelongsTo name) = "BelongsTo " <> show name
  show (HasMany name inverseOf) = "HasMany " <> show name <> " " <> show inverseOf

type Attribute = Attribute' Text

newtype SqlType = SqlType Text
type Name = Text

data DefaultValue stringType = SqlDefaultValue stringType deriving (Show, Eq, Ord)

data OnDelete = NoAction | Restrict | SetNull | Cascade deriving (Show, Eq, Ord)

data FieldType stringType =
             SerialField { defaultValue :: Maybe (DefaultValue stringType), references :: Maybe stringType, allowNull :: Bool, isPrimaryKey :: Bool, onDelete :: OnDelete, unique :: Bool }
           | TextField   { defaultValue :: Maybe (DefaultValue stringType), references :: Maybe stringType, allowNull :: Bool, isPrimaryKey :: Bool, unique :: Bool }
           | IntField    { defaultValue :: Maybe (DefaultValue stringType), references :: Maybe stringType, allowNull :: Bool, isPrimaryKey :: Bool, unique :: Bool }
           | BoolField   { defaultValue :: Maybe (DefaultValue stringType), references :: Maybe stringType, allowNull :: Bool, isPrimaryKey :: Bool, unique :: Bool }
           | EnumField   { defaultValue :: Maybe (DefaultValue stringType), references :: Maybe stringType, values :: [Text], allowNull :: Bool, isPrimaryKey :: Bool, unique :: Bool }
           | UUIDField   { defaultValue :: Maybe (DefaultValue stringType), references :: Maybe stringType, allowNull :: Bool, isPrimaryKey :: Bool, onDelete :: OnDelete, unique :: Bool }
           | Timestamp   { defaultValue :: Maybe (DefaultValue stringType), references :: Maybe stringType, allowNull :: Bool, isPrimaryKey :: Bool, unique :: Bool }
           | PointField  { defaultValue :: Maybe (DefaultValue stringType), references :: Maybe stringType, allowNull :: Bool, isPrimaryKey :: Bool, unique :: Bool }
           deriving (Eq, Ord)

instance (Show stringType) => Show (FieldType stringType) where
    show SerialField { defaultValue, references, allowNull, isPrimaryKey, unique } = "SerialField (" <> show defaultValue <> ") (" <> show references <> ") " <> show allowNull <> " " <> show isPrimaryKey <> " " <> show unique
    show TextField { defaultValue, references, allowNull, isPrimaryKey, unique } = "TextField (" <> show defaultValue <> ") (" <> show references <> ") " <> show allowNull <> " " <> show isPrimaryKey <> " " <> show unique
    show IntField { defaultValue, references, allowNull, isPrimaryKey, unique } = "IntField (" <> show defaultValue <> ") (" <> show references <> ") " <> show allowNull <> " " <> show isPrimaryKey <> " " <> show unique
    show BoolField { defaultValue, references, allowNull, isPrimaryKey, unique } = "BoolField (" <> show defaultValue <> ") (" <> show references <> ") " <> show allowNull <> " " <> show isPrimaryKey <> " " <> show unique
    show UUIDField { defaultValue, references, allowNull, isPrimaryKey, onDelete, unique } = "UUIDField (" <> show defaultValue <> ") (" <> show references <> ") " <> show allowNull <> " " <> show isPrimaryKey <> " " <> show onDelete <> " " <> show unique
    show Timestamp { defaultValue, references, allowNull, isPrimaryKey, unique } = "Timestamp (" <> show defaultValue <> ") (" <> show references <> ") " <> show allowNull <> " " <> show isPrimaryKey <> " " <> show unique
    show PointField { defaultValue, references, allowNull, isPrimaryKey, unique } = "PointField (" <> show defaultValue <> ") (" <> show references <> ") " <> show allowNull <> " " <> show isPrimaryKey <> " " <> show unique

