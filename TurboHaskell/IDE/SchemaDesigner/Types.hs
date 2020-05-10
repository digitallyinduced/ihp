{-|
Module: TurboHaskell.IDE.SchemaDesigner.Types
Description: Types for representing an AST of SQL DDL
Copyright: (c) digitally induced GmbH, 2020
-}
module TurboHaskell.IDE.SchemaDesigner.Types where

import TurboHaskell.Prelude

data Statement
    = 
    -- | CREATE TABLE name ( columns );
    CreateTable { name :: Text, columns :: [Column] }
    -- | CREATE TYPE name AS ENUM ( values );
    | CreateEnumType { name :: Text, values :: [Text] }
    -- | CREATE EXTENSION IF NOT EXISTS "name";
    | CreateExtension { name :: Text, ifNotExists :: Bool }
    -- | ALTER TABLE tableName ADD CONSTRAINT constraintName constraint;
    | AddConstraint { tableName :: Text, constraintName :: Text, constraint :: Constraint }
    | UnknownStatement { raw :: Text }
    | Comment { content :: Text }
    deriving (Eq, Show)

data Column = Column
    { name :: Text
    , columnType :: Text
    , primaryKey :: Bool
    , defaultValue :: Maybe Text
    , notNull :: Bool
    , isUnique :: Bool
    }
    deriving (Eq, Show)

data OnDelete
    = NoAction
    | Restrict
    | SetNull
    | Cascade
    deriving (Show, Eq)

data Constraint
    -- | FOREIGN KEY (columnName) REFERENCES referenceTable (referenceColumn) ON DELETE onDelete;
    = ForeignKeyConstraint
        { columnName :: Text
        , referenceTable :: Text
        , referenceColumn :: Maybe Text
        , onDelete :: Maybe OnDelete
        }
    deriving (Eq, Show)