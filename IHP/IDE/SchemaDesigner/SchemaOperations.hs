{-|
Module: IHP.IDE.SchemaDesigner.SchemaOperations
Description: Apply high level operations to the Schema.sql
Copyright: (c) digitally induced GmbH, 2021
-}
module IHP.IDE.SchemaDesigner.SchemaOperations where

import IHP.Prelude
import IHP.IDE.SchemaDesigner.Types
import Data.Maybe (fromJust)

-- | A Schema.sql basically is just a list of sql DDL statements
type Schema = [Statement]

-- | Creates a new tables with a 'id' columns as the primary key
addTable :: Text -> Schema -> Schema
addTable tableName list = list <> [StatementCreateTable CreateTable
    { name = tableName
    , columns =
        [Column
            { name = "id"
            , columnType = PUUID
            , defaultValue = Just (CallExpression "uuid_generate_v4" [])
            , notNull = True
            , isUnique = False
            }]
    , primaryKeyConstraint = PrimaryKeyConstraint ["id"]
    , constraints = []
    }]


data AddColumnOptions = AddColumnOptions
    { tableName :: !Text
    , columnName :: !Text
    , columnType :: !PostgresType
    , defaultValue :: !(Maybe Expression)
    , isArray :: !Bool
    , allowNull :: !Bool
    , isUnique :: !Bool
    , isReference :: !Bool
    , referenceTable :: !(Maybe Text)
    , primaryKey :: !Bool
    }
addColumn :: AddColumnOptions -> Schema -> Schema
addColumn AddColumnOptions { .. } = 
    let
        column = Column
            { name = columnName
            , columnType = arrayifytype isArray columnType
            , defaultValue = defaultValue
            , notNull = (not allowNull)
            , isUnique = isUnique
            }


        addColumnToTable :: Text -> Column -> Bool -> Statement -> Statement
        addColumnToTable tableName (column@Column { name = columnName }) isPrimaryKey (StatementCreateTable table@CreateTable { name, columns, primaryKeyConstraint = PrimaryKeyConstraint pks})
            | name == tableName =
                let primaryKeyConstraint =
                      if isPrimaryKey
                      then PrimaryKeyConstraint (pks <> [columnName])
                      else PrimaryKeyConstraint pks
                in StatementCreateTable (table { columns = columns <> [column] , primaryKeyConstraint })
        addColumnToTable tableName column isPrimaryKey statement = statement

        addTableOp :: Schema -> Schema = map (addColumnToTable tableName column primaryKey)
    in
        if isReference then
            let
                constraintName = tableName <> "_ref_" <> columnName
                onDelete = NoAction
                addForeignKeyConstraintToSchema = addForeignKeyConstraint tableName columnName constraintName (fromJust referenceTable) onDelete

                indexName = tableName <> "_" <> columnName <> "_index"
                columnNames = [columnName]
                addTableIndexToSchema = addTableIndex indexName False tableName columnNames
            in
                addForeignKeyConstraintToSchema . addTableIndexToSchema . addTableOp
        else
            addTableOp


arrayifytype :: Bool -> PostgresType -> PostgresType
arrayifytype False   (PArray coltype) = coltype
arrayifytype True  a@(PArray coltype) = a
arrayifytype False coltype = coltype
arrayifytype True  coltype = PArray coltype

addForeignKeyConstraint :: Text -> Text -> Text -> Text -> OnDelete -> [Statement] -> [Statement]
addForeignKeyConstraint tableName columnName constraintName referenceTable onDelete list = list <> [AddConstraint { tableName = tableName, constraintName = constraintName, constraint = ForeignKeyConstraint { columnName = columnName, referenceTable = referenceTable, referenceColumn = "id", onDelete = (Just onDelete) } }]

addTableIndex :: Text -> Bool -> Text -> [Text] -> [Statement] -> [Statement]
addTableIndex indexName unique tableName columnNames list = list <> [CreateIndex { indexName, unique, tableName, expressions = map VarExpression columnNames, whereClause = Nothing }]
