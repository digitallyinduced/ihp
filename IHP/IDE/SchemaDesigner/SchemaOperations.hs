{-|
Module: IHP.IDE.SchemaDesigner.SchemaOperations
Description: Apply high level operations to the Schema.sql
Copyright: (c) digitally induced GmbH, 2021
-}
module IHP.IDE.SchemaDesigner.SchemaOperations where

import IHP.Prelude
import IHP.IDE.SchemaDesigner.Types
import Data.Maybe (fromJust)
import qualified Data.List as List

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
addColumn options@(AddColumnOptions { .. }) = 
    let
        column = newColumn options
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

        foreignKeyConstraint = newForeignKeyConstraint tableName columnName (fromJust referenceTable)
        foreignKeyIndex = newForeignKeyIndex tableName columnName
    in
        if isReference then
            \statements -> statements
            |> addTableOp
            |> appendStatement foreignKeyIndex
            |> appendStatement foreignKeyConstraint
        else
            addTableOp

newColumn :: AddColumnOptions -> Column
newColumn AddColumnOptions { .. } = Column
    { name = columnName
    , columnType = arrayifytype isArray columnType
    , defaultValue = defaultValue
    , notNull = (not allowNull)
    , isUnique = isUnique
    }

newForeignKeyConstraint :: Text -> Text -> Text -> Statement
newForeignKeyConstraint tableName columnName referenceTable =
    AddConstraint
    { tableName
    , constraintName = tableName <> "_ref_" <> columnName
    , constraint = ForeignKeyConstraint
        { columnName = columnName
        , referenceTable = referenceTable
        , referenceColumn = "id"
        , onDelete = (Just NoAction)
        }
    }

newForeignKeyIndex :: Text -> Text -> Statement
newForeignKeyIndex tableName columnName =
    CreateIndex
    { indexName = tableName <> "_" <> columnName <> "_index"
    , unique = False
    , tableName
    , expressions = [VarExpression columnName]
    , whereClause = Nothing
    }

appendStatement :: Statement -> [Statement] -> [Statement]
appendStatement statement statements = statements <> [statement]

arrayifytype :: Bool -> PostgresType -> PostgresType
arrayifytype False   (PArray coltype) = coltype
arrayifytype True  a@(PArray coltype) = a
arrayifytype False coltype = coltype
arrayifytype True  coltype = PArray coltype

addForeignKeyConstraint :: Text -> Text -> Text -> Text -> OnDelete -> [Statement] -> [Statement]
addForeignKeyConstraint tableName columnName constraintName referenceTable onDelete list = list <> [AddConstraint { tableName = tableName, constraintName = constraintName, constraint = ForeignKeyConstraint { columnName = columnName, referenceTable = referenceTable, referenceColumn = "id", onDelete = (Just onDelete) } }]

addTableIndex :: Text -> Bool -> Text -> [Text] -> [Statement] -> [Statement]
addTableIndex indexName unique tableName columnNames list = list <> [CreateIndex { indexName, unique, tableName, expressions = map VarExpression columnNames, whereClause = Nothing }]

-- | An enum is added after all existing enum statements, but right before @CREATE TABLE@ statements
addEnum :: Text -> Schema -> Schema
addEnum enumName statements = a <> enum <> b
    where
        enum = [CreateEnumType { name = enumName, values = []}]
        (a, b) = List.splitAt insertionIndex statements
        
        insertionIndex = findInsertionIndex statements 0

        -- Finds the index after comments and existing enum types, just before the CREATE TABLE statements
        findInsertionIndex ((Comment{}):xs) !i = findInsertionIndex xs (i + 1)
        findInsertionIndex ((CreateEnumType{}):xs) !i = findInsertionIndex xs (i + 1)
        findInsertionIndex (x:xs) !i = i
        findInsertionIndex [] !i = i

addValueToEnum :: Text -> Text -> Schema -> Schema
addValueToEnum enumName enumValueName statements = map addValueToEnum' statements
    where
        addValueToEnum' (table@CreateEnumType { name, values }) | name == enumName =
            table { values = values <> [enumValueName] }
        addValueToEnum' statement = statement

data UpdatePolicyOptions = UpdatePolicyOptions
    { currentName :: !Text -- ^ Current name of the policy
    , tableName :: !Text -- ^ Table of the policy
    , name :: !Text -- ^ New name of the policy
    , using :: !(Maybe Expression)
    , check :: !(Maybe Expression)
    }

updatePolicy :: UpdatePolicyOptions -> Schema -> Schema
updatePolicy UpdatePolicyOptions { .. } statements =
        statements
        |> map updatePolicy'
    where
        updatePolicy' policy@CreatePolicy { name = pName, tableName = pTable } | pName == currentName && pTable == tableName = CreatePolicy { tableName, name, using, check }
        updatePolicy' otherwise                                                                                              = otherwise

data AddPolicyOptions = AddPolicyOptions
    { tableName :: !Text
    , name :: !Text
    , using :: !(Maybe Expression)
    , check :: !(Maybe Expression)
    }

addPolicy :: AddPolicyOptions -> Schema -> Schema
addPolicy AddPolicyOptions { .. } statements = statements <> createPolicyStatement
    where
        createPolicyStatement = [ CreatePolicy { tableName, name, using, check } ]

data DeletePolicyOptions = DeletePolicyOptions
    { tableName :: !Text
    , policyName :: !Text
    }

deletePolicy :: DeletePolicyOptions -> Schema -> Schema
deletePolicy DeletePolicyOptions { .. } statements =
        statements
        |> filter (not . isSelectedPolicy)
    where
        isSelectedPolicy :: Statement -> Bool
        isSelectedPolicy policy@CreatePolicy { name = pName, tableName = pTable } = pName == policyName && pTable == tableName
        isSelectedPolicy otherwise                                                = False

enableRowLevelSecurity :: Text -> Schema -> Schema
enableRowLevelSecurity tableName schema =
    let
        rlsEnabled = schema
                |> find \case
                    EnableRowLevelSecurity { tableName = rlsTable } -> rlsTable == tableName
                    otherwise                                       -> False
                |> isJust
    in if rlsEnabled
        then schema
        else schema <> [ EnableRowLevelSecurity { tableName } ]

disableRowLevelSecurity :: Text -> Schema -> Schema
disableRowLevelSecurity tableName schema = schema
        |> filter \case
            EnableRowLevelSecurity { tableName = rlsTable } -> rlsTable /= tableName
            otherwise                                       -> True

disableRowLevelSecurityIfNoPolicies :: Text -> Schema -> Schema
disableRowLevelSecurityIfNoPolicies tableName schema =
    let
        tableHasPolicies = schema
                |> find \case
                    CreatePolicy { tableName = policyTable } -> policyTable == tableName
                    otherwise                                -> False
                |> isJust
    in if tableHasPolicies
        then schema
        else disableRowLevelSecurity tableName schema