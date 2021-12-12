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


-- | Checks if there exists a @user_id@ column, and returns a policy based on that.
-- If there's no @user_id@ field on the table it will return an empty policy
--
-- This function also follows foreign keys to find the shortest path to a user_id.
-- E.g. when having a schema post_meta_tags (no user_id column) <-> posts (has a user_id) <-> users:
--
-- >                                                 post_id
-- >                            posts_meta_infos ────────────────►  posts
-- >                                                                 │
-- >                                                                 │
-- >                                                                 │
-- >                                                                 │
-- >                                                                 │
-- >                                                                 │ user_id
-- >                                                                 │
-- >                                                                 │
-- >                                                                 │
-- >                                                                 │
-- >                                          users  ◄───────────────┘
--
suggestPolicy :: Schema -> Statement -> Statement
suggestPolicy schema (StatementCreateTable CreateTable { name = tableName, columns })
    | isJust (find isUserIdColumn columns)  = CreatePolicy
        { name = "Users can manage their " <> tableName
        , tableName
        , using = Just compareUserId
        , check = Just compareUserId
        }
    where
        compareUserId = EqExpression (VarExpression "user_id") (CallExpression "ihp_user_id" [])
suggestPolicy schema (StatementCreateTable CreateTable { name = tableName, columns }) = 
            columnsWithFKAndRefTable
                |> mapMaybe columnWithFKAndRefTableToPolicy
                |> head
                |> fromMaybe (emptyPolicy)
        where
            referenced = columns

            columnWithFKAndRefTableToPolicy :: (Column, Constraint, CreateTable) -> Maybe Statement
            columnWithFKAndRefTableToPolicy (column, ForeignKeyConstraint { referenceColumn }, CreateTable { name = refTableName, columns = refTableColumns }) | isJust (find isUserIdColumn refTableColumns) = Just CreatePolicy
                    { name = "Users can manage the " <> tableName <> " if they can see the " <> tableNameToModelName refTableName
                    , tableName
                    , using = Just delegateCheck
                    , check = Just delegateCheck
                    }
                where
                    delegateCheck = ExistsExpression (
                            SelectExpression (
                                Select
                                { columns = [IntExpression 1]
                                , from = DotExpression (VarExpression "public") refTableName
                                , whereClause = EqExpression (DotExpression (VarExpression refTableName) refColumnName) (DotExpression (VarExpression tableName) (get #name column))
                                }
                            )
                        )
                    refColumnName = referenceColumn |> fromMaybe "id"

            columnWithFKAndRefTableToPolicy otherwise = Nothing


            columnsWithFKAndRefTable :: [(Column, Constraint, CreateTable)]
            columnsWithFKAndRefTable =
                 columns
                 |> map findFK
                 |> zip columns
                 |> mapMaybe \case
                        (col, Just fk) -> Just (col, fk)
                        (col, Nothing) -> Nothing
                 |> map (\(column, fk) -> (column, fk, resolveFK fk))
                 |> mapMaybe  \case
                        (column, fk, Just refTable) -> Just (column, fk, refTable)
                        (column, fk, Nothing)       -> Nothing

            findFK :: Column -> Maybe Constraint
            findFK column =
                schema
                    |> mapMaybe (\case
                        AddConstraint { tableName = fkTable, constraint = fk@(ForeignKeyConstraint { columnName = fkCol }) } ->
                            if fkTable == tableName && fkCol == get #name column
                                then Just fk
                                else Nothing
                        otherwise -> Nothing)
                    |> head

            resolveFK :: Constraint -> Maybe CreateTable
            resolveFK ForeignKeyConstraint { referenceTable } = schema
                    |> find \case
                        StatementCreateTable CreateTable {  name }  -> name == referenceTable
                        otheriwse                                   -> False
                    |> fmap \case
                        StatementCreateTable table -> table

            emptyPolicy = CreatePolicy { name = "", tableName, using = Nothing, check = Nothing }

isUserIdColumn :: Column -> Bool
isUserIdColumn Column { name = "user_id" } = True
isUserIdColumn otherwise                   = False