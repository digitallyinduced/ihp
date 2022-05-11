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
import qualified Data.Text as Text

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
            , generator = Nothing
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
    , withIndex :: !Bool
    , autoPolicy :: !Bool
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
        index = newColumnIndex tableName columnName

        handleAutoPolicy statements =
            if autoPolicy
                then
                    let
                        isTable (StatementCreateTable CreateTable { name }) = name == tableName
                        isTable otherwise = False
                        (Just table) = find isTable statements
                        suggestedPolicy = suggestPolicy statements table
                    in if (get #name suggestedPolicy /= "" && not (doesHaveExistingPolicies statements tableName))
                            then
                                statements
                                |> enableRowLevelSecurity tableName
                                |> addPolicy AddPolicyOptions
                                        { tableName = tableName
                                        , name = get #name suggestedPolicy
                                        , using = get #using suggestedPolicy
                                        , check = get #check suggestedPolicy
                                        }
                            else statements
                else statements
    in
        if isReference then
            \statements -> statements
            |> addTableOp
            |> appendStatement index
            |> appendStatement foreignKeyConstraint
            |> handleAutoPolicy
        else
            addTableOp
            . (if withIndex
                    then appendStatement index
                    else \schema -> schema)
            . (if columnName == "updated_at"
                then addUpdatedAtTrigger tableName
                else \schema -> schema)

data UpdateColumnOptions = UpdateColumnOptions
    { tableName :: !Text
    , columnName :: !Text
    , columnType :: !PostgresType
    , defaultValue :: !(Maybe Expression)
    , isArray :: !Bool
    , allowNull :: !Bool
    , isUnique :: !Bool
    , primaryKey :: !Bool
    , columnId :: !Int
    }
updateColumn :: UpdateColumnOptions -> Schema -> Schema
updateColumn options@(UpdateColumnOptions { .. }) schema =
    let
        updateColumnAtIndex :: [Column] -> [Column]
        updateColumnAtIndex columns = mapWithIndex updateColumnAtIndex' columns

        mapWithIndex :: (a -> Int -> b) -> [a] -> [b]
        mapWithIndex mapFn items = mapWithIndex' mapFn items 0
            where
                mapWithIndex' :: (a -> Int -> b) -> [a] -> Int -> [b]
                mapWithIndex' mapFn [] _ = []
                mapWithIndex' mapFn (item:rest) i = (mapFn item i):(mapWithIndex' mapFn rest (i + 1))

        updateColumnAtIndex' :: Column -> Int -> Column
        updateColumnAtIndex' column index | index == columnId = column
                { name = columnName
                , columnType = arrayifytype isArray columnType
                , defaultValue = defaultValue
                , notNull = not allowNull
                , isUnique
                }
        updateColumnAtIndex' column index = column

        updateTableOp :: [Statement] -> [Statement]
        updateTableOp = map \case
                (StatementCreateTable table@(CreateTable { name, columns, primaryKeyConstraint })) | name == tableName ->
                    let
                        oldColumn :: Column
                        oldColumn = columns
                                |> (\c -> zip c [0..])
                                |> find ((\(c, index) -> index == columnId))
                                |> fromMaybe (error "could not find column with id")
                                |> fst
                    in StatementCreateTable $ (table :: CreateTable)
                            { columns = updateColumnAtIndex columns
                            , primaryKeyConstraint = updatePrimaryKeyConstraint oldColumn primaryKey primaryKeyConstraint
                            }
                otherwise -> otherwise

        -- | Add or remove a column from the primary key constraint
        updatePrimaryKeyConstraint :: Column -> Bool -> PrimaryKeyConstraint -> PrimaryKeyConstraint
        updatePrimaryKeyConstraint Column { name } isPrimaryKey primaryKeyConstraint@PrimaryKeyConstraint { primaryKeyColumnNames } =
          case (isPrimaryKey, name `elem` primaryKeyColumnNames) of
              (False, False) -> primaryKeyConstraint
              (False, True) -> PrimaryKeyConstraint (filter (/= name) primaryKeyColumnNames)
              (True, False) -> PrimaryKeyConstraint (primaryKeyColumnNames <> [name])
              (True, True) -> primaryKeyConstraint

        updateForeignKeyConstraints = map \case
                statement@(AddConstraint { tableName = constraintTable, constraint = constraint@(ForeignKeyConstraint { name = fkName, columnName = fkColumnName  })  }) | constraintTable == tableName && fkColumnName == (get #name oldColumn) ->
                    let newName = Text.replace (get #name oldColumn) columnName <$> fkName
                    in statement { constraint = constraint { columnName, name = newName } }
                index@(CreateIndex { indexName, tableName = indexTable, columns = indexColumns }) | indexTable == tableName ->
                    let
                        updateIndexColumn :: IndexColumn -> IndexColumn
                        updateIndexColumn indexColumn@(IndexColumn { column = VarExpression varName }) | varName == (get #name oldColumn) = indexColumn { column = VarExpression columnName }
                        updateIndexColumn otherwise = otherwise
                    in
                        (index :: Statement) { columns = map updateIndexColumn indexColumns, indexName = Text.replace (get #name oldColumn) columnName indexName }
                otherwise -> otherwise
        findOldColumn statements = mapMaybe findOldColumn' statements
                |> head
                |> fromMaybe (error "Could not find old column")
        findOldColumn' (StatementCreateTable table@(CreateTable { name, columns, primaryKeyConstraint })) | name == tableName =
                    let
                        oldColumn :: Column
                        oldColumn = columns
                                |> (\c -> zip c [0..])
                                |> find ((\(c, index) -> index == columnId))
                                |> fromMaybe (error "could not find column with id")
                                |> fst
                    in
                        Just oldColumn
        findOldColumn' _ = Nothing

        oldColumn :: Column
        oldColumn = findOldColumn schema
    in
        schema
        |> updateTableOp
        |> updateForeignKeyConstraints

newColumn :: AddColumnOptions -> Column
newColumn AddColumnOptions { .. } = Column
    { name = columnName
    , columnType = arrayifytype isArray columnType
    , defaultValue = defaultValue
    , notNull = (not allowNull)
    , isUnique = isUnique
    , generator = Nothing
    }

newForeignKeyConstraint :: Text -> Text -> Text -> Statement
newForeignKeyConstraint tableName columnName referenceTable =
    AddConstraint
    { tableName
    , constraint = ForeignKeyConstraint
        { name = Just $ tableName <> "_ref_" <> columnName
        , columnName = columnName
        , referenceTable = referenceTable
        , referenceColumn = "id"
        , onDelete = (Just NoAction)
        }
    , deferrable = Nothing
    , deferrableType = Nothing
    }

newColumnIndex :: Text -> Text -> Statement
newColumnIndex tableName columnName =
    CreateIndex
    { indexName = tableName <> "_" <> columnName <> "_index"
    , unique = False
    , tableName
    , columns = [IndexColumn { column = VarExpression columnName, columnOrder = [] }]
    , whereClause = Nothing
    , indexType = Nothing
    }

appendStatement :: Statement -> [Statement] -> [Statement]
appendStatement statement statements = statements <> [statement]

arrayifytype :: Bool -> PostgresType -> PostgresType
arrayifytype False   (PArray coltype) = coltype
arrayifytype True  a@(PArray coltype) = a
arrayifytype False coltype = coltype
arrayifytype True  coltype = PArray coltype

addForeignKeyConstraint :: Text -> Text -> Text -> Text -> OnDelete -> [Statement] -> [Statement]
addForeignKeyConstraint tableName columnName constraintName referenceTable onDelete list = list <> [AddConstraint { tableName = tableName, constraint = ForeignKeyConstraint { name = Just constraintName, columnName = columnName, referenceTable = referenceTable, referenceColumn = "id", onDelete = (Just onDelete) }, deferrable = Nothing, deferrableType = Nothing }]

addTableIndex :: Text -> Bool -> Text -> [Text] -> [Statement] -> [Statement]
addTableIndex indexName unique tableName columnNames list = list <> [CreateIndex { indexName, unique, tableName, columns = columnNames |> map (\columnName -> IndexColumn { column = VarExpression columnName, columnOrder = [] }), whereClause = Nothing, indexType = Nothing }]

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
        updatePolicy' policy@CreatePolicy { name = pName, action, tableName = pTable } | pName == currentName && pTable == tableName = CreatePolicy { tableName, action, name, using, check }
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
        createPolicyStatement = [ CreatePolicy { tableName, action = Nothing, name, using, check } ]

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
        , action = Nothing
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
                    , action = Nothing
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
                                , alias = Nothing
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

            emptyPolicy = CreatePolicy { name = "", action = Nothing, tableName, using = Nothing, check = Nothing }

isUserIdColumn :: Column -> Bool
isUserIdColumn Column { name = "user_id" } = True
isUserIdColumn otherwise                   = False


deleteTable :: Text -> Schema -> Schema
deleteTable tableName statements =
    statements
    |> filter \case
        StatementCreateTable CreateTable { name }       | name == tableName            -> False
        AddConstraint { tableName = constraintTable }   | constraintTable == tableName -> False
        CreateIndex { tableName = indexTable }          | indexTable == tableName      -> False
        EnableRowLevelSecurity { tableName = rlsTable } | rlsTable == tableName        -> False
        CreatePolicy { tableName = policyTable }        | policyTable == tableName     -> False
        CreateTrigger { tableName = triggerTable }      | triggerTable == tableName    -> False
        otherwise -> True

updatedAtTriggerName :: Text -> Text
updatedAtTriggerName tableName = "update_" <> tableName <> "_updated_at"

addUpdatedAtTrigger :: Text -> [Statement] -> [Statement]
addUpdatedAtTrigger tableName schema =
        addFunctionOperator <> schema <> [trigger]
    where
        trigger :: Statement
        trigger = CreateTrigger
            { name = updatedAtTriggerName tableName
            , eventWhen = Before
            , event = TriggerOnUpdate
            , tableName
            , for = ForEachRow
            , whenCondition = Nothing
            , functionName = get #functionName setUpdatedAtToNowTrigger
            , arguments = []
            }

        addFunctionOperator :: [Statement]
        addFunctionOperator =
            if hasFunction (get #functionName setUpdatedAtToNowTrigger)
                then []
                else [setUpdatedAtToNowTrigger]

        hasFunction :: Text -> Bool
        hasFunction name = schema
                |> find \case
                    CreateFunction { functionName = fnName } -> name == fnName
                    otherwise -> False
                |> isJust

        setUpdatedAtToNowTrigger :: Statement
        setUpdatedAtToNowTrigger = 
            CreateFunction
                { functionName = "set_updated_at_to_now"
                , functionBody = "\n" <> [trimming|
                    BEGIN
                        NEW.updated_at = NOW();
                        RETURN NEW;
                    END;
                |] <> "\n"
                , functionArguments = []
                , orReplace = False
                , returns = PTrigger
                , language = "plpgsql"
                }

deleteTriggerIfExists :: Text -> [Statement] -> [Statement]
deleteTriggerIfExists triggerName statements = filter (not . isTheTriggerToBeDeleted) statements
    where
        isTheTriggerToBeDeleted CreateTrigger { name } = triggerName == name
        isTheTriggerToBeDeleted _                      = False

data DeleteColumnOptions
    = DeleteColumnOptions
    { tableName :: !Text
    , columnName :: !Text
    , columnId :: !Int
    }

deleteColumn :: DeleteColumnOptions -> Schema -> Schema
deleteColumn DeleteColumnOptions { .. } schema =
        schema
        |> map deleteColumnInTable
        |> (filter \case
                AddConstraint { tableName = fkTable, constraint = ForeignKeyConstraint { columnName = fkColumn } } | fkTable == tableName && fkColumn == columnName -> False
                index@(CreateIndex {}) | isIndexStatementReferencingTableColumn index tableName columnName -> False
                otherwise -> True
            )
        |> (if columnName == "updated_at"
                then deleteTriggerIfExists (updatedAtTriggerName tableName)
                else \schema -> schema
            )
    where
        deleteColumnInTable :: Statement -> Statement
        deleteColumnInTable (StatementCreateTable table@CreateTable { name, columns }) | name == tableName = StatementCreateTable $ table { columns = delete (columns !! columnId) columns}
        deleteColumnInTable statement = statement

-- | Returns True if a CreateIndex statement references a specific column
--
-- E.g. given a schema like this:
-- > CREATE TABLE users (
-- >     email TEXT NOT NULL
-- > );
-- >
-- > CREATE UNIQUE INDEX users_email_index ON users (LOWER(email));
-- >
--
-- You can find all indices to the email column of the users table like this:
--
-- >>> filter (isIndexStatementReferencingTableColumn "users" "email") database
-- [CreateIndex { indexName = "users_email", unique = True, tableName = "users", expressions = [CallExpression "LOWER" [VarEpression "email"]] }]
--
isIndexStatementReferencingTableColumn :: Statement -> Text -> Text -> Bool
isIndexStatementReferencingTableColumn statement tableName columnName = isReferenced statement
    where
        -- | Returns True if a statement is an CreateIndex statement that references our specific column
        --
        -- An index references a table if it references the target table and one of the index expressions contains a reference to our column
        isReferenced :: Statement -> Bool
        isReferenced CreateIndex { tableName = indexTableName, columns } = indexTableName == tableName && expressionsReferencesColumn (map (get #column) columns)
        isReferenced otherwise = False

        -- | Returns True if a list of expressions references the columnName
        expressionsReferencesColumn :: [Expression] -> Bool
        expressionsReferencesColumn expressions = expressions
                |> map expressionReferencesColumn
                |> List.or

        -- | Walks the expression tree and returns True if there's a VarExpression with the column name
        expressionReferencesColumn :: Expression -> Bool
        expressionReferencesColumn = \case
            TextExpression _ -> False
            VarExpression varName -> varName == columnName
            CallExpression _ expressions -> expressions
                    |> map expressionReferencesColumn
                    |> List.or
            NotEqExpression a b -> expressionReferencesColumn a || expressionReferencesColumn b
            EqExpression a b -> expressionReferencesColumn a || expressionReferencesColumn b
            AndExpression a b -> expressionReferencesColumn a || expressionReferencesColumn b
            IsExpression a b -> expressionReferencesColumn a || expressionReferencesColumn b
            NotExpression a -> expressionReferencesColumn a
            OrExpression a b -> expressionReferencesColumn a || expressionReferencesColumn b
            LessThanExpression a b -> expressionReferencesColumn a || expressionReferencesColumn b
            LessThanOrEqualToExpression a b -> expressionReferencesColumn a || expressionReferencesColumn b
            GreaterThanExpression a b -> expressionReferencesColumn a || expressionReferencesColumn b
            GreaterThanOrEqualToExpression a b -> expressionReferencesColumn a || expressionReferencesColumn b

doesHaveExistingPolicies :: [Statement] -> Text -> Bool
doesHaveExistingPolicies statements tableName = statements
                |> find \case
                    CreatePolicy { tableName = tableName' } -> tableName' == tableName
                    otherwise                               -> False
                |> isJust