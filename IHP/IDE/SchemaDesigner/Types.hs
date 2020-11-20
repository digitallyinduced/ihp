{-|
Module: IHP.IDE.SchemaDesigner.Types
Description: Types for representing an AST of SQL DDL
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.IDE.SchemaDesigner.Types where

import IHP.Prelude

data Statement
    =
    -- | CREATE TABLE name ( columns );
      StatementCreateTable { unsafeGetCreateTable :: CreateTable }
    -- | CREATE TYPE name AS ENUM ( values );
    | CreateEnumType { name :: Text, values :: [Text] }
    -- | CREATE EXTENSION IF NOT EXISTS "name";
    | CreateExtension { name :: Text, ifNotExists :: Bool }
    -- | ALTER TABLE tableName ADD CONSTRAINT constraintName constraint;
    | AddConstraint { tableName :: Text, constraintName :: Text, constraint :: Constraint }
    | UnknownStatement { raw :: Text }
    | Comment { content :: Text }
    deriving (Eq, Show)

data CreateTable
  = CreateTable
      { name :: Text
      , columns :: [Column]
      , primaryKeyConstraint :: PrimaryKeyConstraint
      , constraints :: [Constraint]
      }
  deriving (Eq, Show)

data Column = Column
    { name :: Text
    , columnType :: PostgresType
    , defaultValue :: Maybe Expression
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

newtype PrimaryKeyConstraint
  = PrimaryKeyConstraint { primaryKeyColumnNames :: [Text] }
  deriving (Eq, Show)

data Constraint
    -- | FOREIGN KEY (columnName) REFERENCES referenceTable (referenceColumn) ON DELETE onDelete;
    = ForeignKeyConstraint
        { columnName :: Text
        , referenceTable :: Text
        , referenceColumn :: Maybe Text
        , onDelete :: Maybe OnDelete
        }
    | UniqueConstraint
        { columnNames :: [Text] }
    deriving (Eq, Show)

data Expression =
    -- | Sql string like @'hello'@
    TextExpression Text
    -- | Simple variable like @users@
    | VarExpression Text
    -- | Simple call, like @COALESCE(name, 'unknown name')@
    | CallExpression Text [Expression]
    deriving (Eq, Show)

data PostgresType
    = PUUID
    | PText
    | PInt
    | PBigInt
    | PBoolean
    | PTimestampWithTimezone
    | PTimestamp
    | PReal
    | PDouble
    | PPoint
    | PDate
    | PBinary
    | PTime
    | PNumeric { precision :: Maybe Int, scale :: Maybe Int }
    | PVaryingN Int
    | PCharacterN Int
    | PSerial
    | PBigserial
    | PJSONB
    | PArray PostgresType
    | PCustomType Text
    deriving (Eq, Show)