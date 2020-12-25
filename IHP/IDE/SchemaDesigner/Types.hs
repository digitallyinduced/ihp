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

instance Ord Statement where
    compare (CreateExtension name1 ifNotExists1) (CreateExtension name2 ifNotExists2) =
        compare name1 name2 <> compare ifNotExists1 ifNotExists2
    compare CreateExtension{..} _ = GT
    compare _ CreateExtension{..} = GT
    compare (CreateEnumType name1 values1) (CreateEnumType name2 values2) =
        compare name1 name2 <> compare (sort values1) (sort values2)
    compare CreateEnumType{..} _ = GT
    compare _ CreateEnumType{..} = GT
    compare (StatementCreateTable unsafeGetCreateTable1) (StatementCreateTable unsafeGetCreateTable2) = compare unsafeGetCreateTable1 unsafeGetCreateTable2
    compare StatementCreateTable{..} _ = GT
    compare _ StatementCreateTable{..} = GT
    compare (AddConstraint tableName1 constraintName1 constraint1) (AddConstraint tableName2 constraintName2 constraint2) =
        compare tableName1 tableName2 <> compare constraintName1 constraintName2 <> compare constraint1 constraint2
    compare AddConstraint{..} _ = GT
    compare _ AddConstraint{..} = GT
    compare (UnknownStatement raw1) (UnknownStatement raw2) =
        compare raw1 raw2
    compare UnknownStatement{..} _ = GT
    compare _ UnknownStatement{..} = GT
    compare (Comment content1) (Comment content2) = compare content1 content2

data CreateTable
  = CreateTable
      { name :: Text
      , columns :: [Column]
      , primaryKeyConstraint :: PrimaryKeyConstraint
      , constraints :: [Constraint]
      }
  deriving (Eq, Show, Ord)

data Column = Column
    { name :: Text
    , columnType :: PostgresType
    , defaultValue :: Maybe Expression
    , notNull :: Bool
    , isUnique :: Bool
    }
    deriving (Eq, Show, Ord)

data OnDelete
    = NoAction
    | Restrict
    | SetNull
    | SetDefault
    | Cascade
    deriving (Show, Eq, Ord)

newtype PrimaryKeyConstraint
  = PrimaryKeyConstraint { primaryKeyColumnNames :: [Text] }
  deriving (Eq, Show, Ord)

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
    deriving (Eq, Show, Ord)

data Expression =
    -- | Sql string like @'hello'@
    TextExpression Text
    -- | Simple variable like @users@
    | VarExpression Text
    -- | Simple call, like @COALESCE(name, 'unknown name')@
    | CallExpression Text [Expression]
    deriving (Eq, Show, Ord)

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
    deriving (Eq, Show, Ord)
