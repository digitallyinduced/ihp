{-# LANGUAGE PolyKinds, UndecidableInstances  #-}
module TurboHaskell.SchemaTypes where

import ClassyPrelude hiding (length)
import Data.Maybe (fromJust)
import qualified Data.List as List
import GHC.Types

import Data.UUID
import TurboHaskell.DatabaseSupport.Point

data Table = Table Text [Attribute]
           deriving (Show, Eq, Ord)

data Attribute = Field Text FieldType
               | BelongsTo Text
               | HasMany { name :: Text, inverseOf :: Maybe Text }
               deriving (Eq, Ord)

instance Show Attribute where
  show (Field fieldName fieldType) = "Field " <> show fieldName <> " (" <> show fieldType <> ")"
  show (BelongsTo name) = "BelongsTo " <> show name
  show (HasMany name inverseOf) = "HasMany " <> show name <> " " <> "(" <> show inverseOf <> ")"

newtype SqlType = SqlType Text
type Name = Text

data DefaultValue = SqlDefaultValue !Text deriving (Show, Eq, Ord)

data OnDelete = NoAction | Restrict | SetNull | Cascade deriving (Show, Eq, Ord)

data FieldType =
             SerialField { defaultValue :: Maybe DefaultValue, references :: Maybe Text, allowNull :: Bool, isPrimaryKey :: Bool, onDelete :: OnDelete, unique :: Bool }
           | TextField   { defaultValue :: Maybe DefaultValue, references :: Maybe Text, allowNull :: Bool, isPrimaryKey :: Bool, unique :: Bool }
           | IntField    { defaultValue :: Maybe DefaultValue, references :: Maybe Text, allowNull :: Bool, isPrimaryKey :: Bool, unique :: Bool }
           | BoolField   { defaultValue :: Maybe DefaultValue, references :: Maybe Text, allowNull :: Bool, isPrimaryKey :: Bool, unique :: Bool }
           | EnumField   { defaultValue :: Maybe DefaultValue, references :: Maybe Text, values :: [Text], allowNull :: Bool, isPrimaryKey :: Bool, unique :: Bool }
           | UUIDField   { defaultValue :: Maybe DefaultValue, references :: Maybe Text, allowNull :: Bool, isPrimaryKey :: Bool, onDelete :: OnDelete, unique :: Bool }
           | Timestamp   { defaultValue :: Maybe DefaultValue, references :: Maybe Text, allowNull :: Bool, isPrimaryKey :: Bool, unique :: Bool }
           | PointField  { defaultValue :: Maybe DefaultValue, references :: Maybe Text, allowNull :: Bool, isPrimaryKey :: Bool, unique :: Bool }
           | FloatField  { defaultValue :: Maybe DefaultValue, references :: Maybe Text, allowNull :: Bool, isPrimaryKey :: Bool, unique :: Bool }
           | DoubleField  { defaultValue :: Maybe DefaultValue, references :: Maybe Text, allowNull :: Bool, isPrimaryKey :: Bool, unique :: Bool }
           deriving (Eq, Ord)

instance Show FieldType where
    show SerialField { defaultValue, references, allowNull, isPrimaryKey, unique } = "SerialField (" <> show defaultValue <> ") (" <> show references <> ") " <> show allowNull <> " " <> show isPrimaryKey <> " " <> show unique
    show TextField { defaultValue, references, allowNull, isPrimaryKey, unique } = "TextField (" <> show defaultValue <> ") (" <> show references <> ") " <> show allowNull <> " " <> show isPrimaryKey <> " " <> show unique
    show IntField { defaultValue, references, allowNull, isPrimaryKey, unique } = "IntField (" <> show defaultValue <> ") (" <> show references <> ") " <> show allowNull <> " " <> show isPrimaryKey <> " " <> show unique
    show BoolField { defaultValue, references, allowNull, isPrimaryKey, unique } = "BoolField (" <> show defaultValue <> ") (" <> show references <> ") " <> show allowNull <> " " <> show isPrimaryKey <> " " <> show unique
    show UUIDField { defaultValue, references, allowNull, isPrimaryKey, onDelete, unique } = "UUIDField (" <> show defaultValue <> ") (" <> show references <> ") " <> show allowNull <> " " <> show isPrimaryKey <> " " <> show onDelete <> " " <> show unique
    show Timestamp { defaultValue, references, allowNull, isPrimaryKey, unique } = "Timestamp (" <> show defaultValue <> ") (" <> show references <> ") " <> show allowNull <> " " <> show isPrimaryKey <> " " <> show unique
    show PointField { defaultValue, references, allowNull, isPrimaryKey, unique } = "PointField (" <> show defaultValue <> ") (" <> show references <> ") " <> show allowNull <> " " <> show isPrimaryKey <> " " <> show unique
    show EnumField { values, defaultValue, references, allowNull, isPrimaryKey, unique } = "EnumField (" <> show defaultValue <> ") (" <> show references <> ") (" <> show values <> ") " <> show allowNull <> " " <> show isPrimaryKey <> " " <> show unique
    show FloatField { defaultValue, references, allowNull, isPrimaryKey, unique } = "FloatField (" <> show defaultValue <> ") (" <> show references <> ") " <> show allowNull <> " " <> show isPrimaryKey <> " " <> show unique
    show DoubleField { defaultValue, references, allowNull, isPrimaryKey, unique } = "DoubleField (" <> show defaultValue <> ") (" <> show references <> ") " <> show allowNull <> " " <> show isPrimaryKey <> " " <> show unique

