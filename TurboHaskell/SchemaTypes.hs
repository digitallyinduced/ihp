{-# LANGUAGE PolyKinds, UndecidableInstances  #-}
module TurboHaskell.SchemaTypes where

import ClassyPrelude hiding (length)
import Data.Maybe (fromJust)
import qualified Data.List as List
import GHC.Types

import Data.UUID
import TurboHaskell.DatabaseSupport.Point
import Data.String.Conversions (cs)

data Table = Table Text [Attribute]
           deriving (Show, Eq, Ord)

data Attribute = Field { name :: Text, fieldType :: FieldType }
               | BelongsTo { name :: Text }
               | HasMany { name :: Text, inverseOf :: Maybe Text }
               deriving (Eq, Ord, Show)

newtype SqlType = SqlType Text
type Name = Text

data DefaultValue = SqlDefaultValue !Text
                  | DefaultValue !Text
                  deriving (Show, Eq, Ord)

instance IsString DefaultValue where
  fromString value = DefaultValue (cs value)

data OnDelete = NoAction | Restrict | SetNull | Cascade deriving (Show, Eq, Ord)

data FieldType =
             SerialField { defaultValue :: Maybe DefaultValue, references :: Maybe Text, allowNull :: Bool, isPrimaryKey :: Bool, onDelete :: OnDelete, unique :: Bool }
           | TextField   { defaultValue :: Maybe DefaultValue, references :: Maybe Text, allowNull :: Bool, isPrimaryKey :: Bool, unique :: Bool }
           | IntField    { defaultValue :: Maybe DefaultValue, references :: Maybe Text, allowNull :: Bool, isPrimaryKey :: Bool, unique :: Bool }
           | BoolField   { defaultValue :: Maybe DefaultValue, references :: Maybe Text, allowNull :: Bool, isPrimaryKey :: Bool, unique :: Bool }
           | EnumField   { defaultValue :: Maybe DefaultValue, references :: Maybe Text, allowNull :: Bool, isPrimaryKey :: Bool, unique :: Bool, values :: [Text] }
           | UUIDField   { defaultValue :: Maybe DefaultValue, references :: Maybe Text, allowNull :: Bool, isPrimaryKey :: Bool, onDelete :: OnDelete, unique :: Bool }
           | Timestamp   { defaultValue :: Maybe DefaultValue, references :: Maybe Text, allowNull :: Bool, isPrimaryKey :: Bool, unique :: Bool }
           | PointField  { defaultValue :: Maybe DefaultValue, references :: Maybe Text, allowNull :: Bool, isPrimaryKey :: Bool, unique :: Bool }
           | FloatField  { defaultValue :: Maybe DefaultValue, references :: Maybe Text, allowNull :: Bool, isPrimaryKey :: Bool, unique :: Bool }
           | DoubleField  { defaultValue :: Maybe DefaultValue, references :: Maybe Text, allowNull :: Bool, isPrimaryKey :: Bool, unique :: Bool }
           deriving (Eq, Ord, Show)