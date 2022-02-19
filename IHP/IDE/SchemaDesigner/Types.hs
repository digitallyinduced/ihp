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
    -- | DROP TYPE name;
    | DropEnumType { name :: Text }
    -- | CREATE EXTENSION IF NOT EXISTS "name";
    | CreateExtension { name :: Text, ifNotExists :: Bool }
    -- | ALTER TABLE tableName ADD CONSTRAINT constraint;
    | AddConstraint { tableName :: Text, constraint :: Constraint }
    -- | ALTER TABLE tableName DROP CONSTRAINT constraintName;
    | DropConstraint { tableName, constraintName :: Text }
    -- | ALTER TABLE tableName ADD COLUMN column;
    | AddColumn { tableName :: Text, column :: Column }
    -- | ALTER TABLE tableName DROP COLUMN columnName;
    | DropColumn { tableName :: Text, columnName :: Text }
    -- | DROP TABLE tableName;
    | DropTable { tableName :: Text }
    | UnknownStatement { raw :: Text }
    | Comment { content :: Text }
    -- | CREATE INDEX indexName ON tableName (columnName); CREATE INDEX indexName ON tableName (LOWER(columnName));
    -- | CREATE UNIQUE INDEX name ON table (column [, ...]);
    | CreateIndex { indexName :: Text, unique :: Bool, tableName :: Text, expressions :: [Expression], whereClause :: Maybe Expression }
    -- | DROP INDEX indexName;
    | DropIndex { indexName :: Text }
    -- | CREATE OR REPLACE FUNCTION functionName() RETURNS TRIGGER AS $$functionBody$$ language plpgsql;
    | CreateFunction { functionName :: Text, functionBody :: Text, orReplace :: Bool, returns :: PostgresType, language :: Text }
    -- | ALTER TABLE tableName ENABLE ROW LEVEL SECURITY;
    | EnableRowLevelSecurity { tableName :: Text }
    -- CREATE POLICY name ON tableName USING using WITH CHECK check;
    | CreatePolicy { name :: Text, tableName :: Text, action :: Maybe PolicyAction, using :: Maybe Expression, check :: Maybe Expression }
    -- SET name = value;
    | Set { name :: Text, value :: Expression }
    -- SELECT query;
    | SelectStatement { query :: Text }
    -- CREATE SEQUENCE name;
    | CreateSequence { name :: Text }
    -- ALTER TABLE tableName RENAME COLUMN from TO to;
    | RenameColumn { tableName :: Text, from :: Text, to :: Text }
    -- ALTER TYPE enumName ADD VALUE newValue;
    | AddValueToEnumType { enumName :: Text, newValue :: Text, ifNotExists :: Bool }
    -- ALTER TABLE tableName ALTER COLUMN columnName DROP NOT NULL;
    | DropNotNull { tableName :: Text, columnName :: Text }
    -- ALTER TABLE tableName ALTER COLUMN columnName SET NOT NULL;
    | SetNotNull { tableName :: Text, columnName :: Text }
    -- | ALTER TABLE from RENAME TO to;
    | RenameTable { from :: Text, to :: Text }
    -- | DROP POLICY policyName ON tableName;
    | DropPolicy { tableName :: Text, policyName :: Text }
    -- ALTER TABLE tableName ALTER COLUMN columnName SET DEFAULT 'value';
    | SetDefaultValue { tableName :: Text, columnName :: Text, value :: Expression }
    -- ALTER TABLE tableName ALTER COLUMN columnName DROP DEFAULT;
    | DropDefaultValue { tableName :: Text, columnName :: Text }
    -- | CREATE TRIGGER ..;
    | CreateTrigger { name :: !Text, eventWhen :: !TriggerEventWhen, event :: !TriggerEvent, tableName :: !Text, for :: !TriggerFor, whenCondition :: Maybe Expression, functionName :: !Text, arguments :: ![Expression] }
    -- | BEGIN;
    | Begin
    -- | COMMIT;
    | Commit
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
    , generator :: Maybe ColumnGenerator
    }
    deriving (Eq, Show)

data OnDelete
    = NoAction
    | Restrict
    | SetNull
    | SetDefault
    | Cascade
    deriving (Show, Eq)

data ColumnGenerator
    = ColumnGenerator
    { generate :: !Expression
    , stored :: !Bool
    } deriving (Show, Eq)

newtype PrimaryKeyConstraint
  = PrimaryKeyConstraint { primaryKeyColumnNames :: [Text] }
  deriving (Eq, Show)

data Constraint
    -- | FOREIGN KEY (columnName) REFERENCES referenceTable (referenceColumn) ON DELETE onDelete;
    = ForeignKeyConstraint
        { name :: !(Maybe Text)
        , columnName :: !Text
        , referenceTable :: !Text
        , referenceColumn :: !(Maybe Text)
        , onDelete :: !(Maybe OnDelete)
        }
    | UniqueConstraint
        { name :: !(Maybe Text)
        , columnNames :: ![Text]
        }
    | CheckConstraint
        { name :: !(Maybe Text)
        , checkExpression :: !Expression
        }
    | AlterTableAddPrimaryKey
        { name :: !(Maybe Text)
        , primaryKeyConstraint :: !PrimaryKeyConstraint
        }
    deriving (Eq, Show)

data Expression =
    -- | Sql string like @'hello'@
    TextExpression Text
    -- | Simple variable like @users@
    | VarExpression Text
    -- | Simple call, like @COALESCE(name, 'unknown name')@
    | CallExpression Text [Expression]
    -- | Not equal operator, a <> b
    | NotEqExpression Expression Expression
    -- | Equal operator, a = b
    | EqExpression Expression Expression
    -- | a AND b
    | AndExpression Expression Expression
    -- | a IS b
    | IsExpression Expression Expression
    -- | NOT a
    | NotExpression Expression
    -- | EXISTS a
    | ExistsExpression Expression
    -- | a OR b
    | OrExpression Expression Expression
    -- | a < b
    | LessThanExpression Expression Expression
    -- | a <= b
    | LessThanOrEqualToExpression Expression Expression
    -- | a > b
    | GreaterThanExpression Expression Expression
    -- | a >= b
    | GreaterThanOrEqualToExpression Expression Expression
    -- | Double literal value, e.g. 0.1337
    | DoubleExpression Double
    -- | Integer literal value, e.g. 1337
    | IntExpression Int
    -- | value::type
    | TypeCastExpression Expression PostgresType
    | SelectExpression Select
    | DotExpression Expression Text
    | ConcatenationExpression Expression Expression -- ^ a || b
    deriving (Eq, Show)

data Select = Select
    { columns :: [Expression]
    , from :: Expression
    , alias :: Maybe Text
    , whereClause :: Expression
    } deriving (Eq, Show)

data PostgresType
    = PUUID
    | PText
    | PInt
    | PSmallInt
    | PBigInt
    | PBoolean
    | PTimestampWithTimezone
    | PTimestamp
    | PReal
    | PDouble
    | PPoint
    | PPolygon
    | PDate
    | PBinary
    | PTime
    | PNumeric { precision :: Maybe Int, scale :: Maybe Int }
    | PVaryingN (Maybe Int)
    | PCharacterN Int
    | PSerial
    | PBigserial
    | PJSONB
    | PInet
    | PTSVector
    | PArray PostgresType
    | PTrigger
    | PCustomType Text
    deriving (Eq, Show)

data TriggerEventWhen
    = Before
    | After
    | InsteadOf
    deriving (Eq, Show)

data TriggerEvent
    = TriggerOnInsert
    | TriggerOnUpdate
    | TriggerOnDelete
    | TriggerOnTruncate
    deriving (Eq, Show)

data TriggerFor
    = ForEachRow
    | ForEachStatement
    deriving (Eq, Show)

data PolicyAction
    = PolicyForAll
    | PolicyForSelect
    | PolicyForInsert
    | PolicyForUpdate
    | PolicyForDelete
    deriving (Eq, Show)