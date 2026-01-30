module IHP.GraphQL.Compiler where

import IHP.Prelude
import IHP.GraphQL.Types

import qualified Hasql.DynamicStatements.Snippet as Snippet
import Hasql.DynamicStatements.Snippet (Snippet)
import Prelude (Semigroup (..))
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HashMap
import Text.Countable (singularize, pluralize)

-- | Helper to embed a SQL identifier (double-quoted) as a Snippet
sqlIdentifier :: Text -> Snippet
sqlIdentifier name = Snippet.sql ("\"" <> cs name <> "\"")

compileDocument :: Variables -> Document -> [Snippet]
compileDocument (Variables arguments) document@(Document { definitions = (definition:rest) }) =
    case definition of
        ExecutableDefinition { operation = OperationDefinition { operationType = Query } } ->
            [ Snippet.sql "SELECT to_json(_root.data) FROM (" <> compileDefinition document definition arguments <> Snippet.sql ") AS _root" ]
        ExecutableDefinition { operation = OperationDefinition { operationType = Mutation } } ->
            compileMutationDefinition definition arguments

compileDefinition :: Document -> Definition -> [Argument] -> Snippet
compileDefinition document ExecutableDefinition { operation = OperationDefinition { operationType = Query, selectionSet } } variables =
    selectionSet
    |> map (compileSelection document variables)
    |> unionAll

compileMutationDefinition :: Definition -> [Argument] -> [Snippet]
compileMutationDefinition ExecutableDefinition { operation = OperationDefinition { operationType = Mutation, selectionSet } } arguments =
    selectionSet
    |> map (compileMutationSelection arguments)

compileSelection :: Document -> [Argument] -> Selection -> Snippet
compileSelection document variables field@(Field { alias, name = fieldName, arguments }) =
        Snippet.sql "(SELECT json_build_object(" <> Snippet.param nameOrAlias <> Snippet.sql ", json_agg(" <> sqlIdentifier subqueryId <> Snippet.sql ".*)) AS data FROM (SELECT "
        <> selectQueryPieces document (sqlIdentifier tableName) field
        <> Snippet.sql " FROM " <> sqlIdentifier tableName
        <> joins
        <> where_
        <> Snippet.sql ") AS " <> sqlIdentifier subqueryId <> Snippet.sql ")"
    where
        subqueryId = "_" <> fieldName
        nameOrAlias = fromMaybe fieldName alias

        tableName = if isJust idArgument
            then pluralize fieldName
            else fieldName

        where_ :: Snippet
        where_ = case idArgument of
                Just id -> Snippet.sql " WHERE id = " <> valueToSQL (resolveVariables id variables)
                Nothing -> mempty

        idArgument :: Maybe Value
        idArgument = case arguments of
                [Argument { argumentName = "id", argumentValue }] -> Just argumentValue
                _ -> Nothing


        isJoinField :: Selection -> Bool
        isJoinField Field { selectionSet } = not (null selectionSet)
        isJoinField FragmentSpread {} = False -- TODO: Also support fragment spreads in joined tables

        joins :: Snippet
        joins = field.selectionSet
                |> filter isJoinField
                |> map (fieldToJoin document tableName)
                |> \case
                    [] -> mempty
                    joins -> Snippet.sql " " <> spaceSep joins


selectQueryPieces :: Document -> Snippet -> Selection -> Snippet
selectQueryPieces document tableName field = field.selectionSet
        |> map compileSelection
        |> mconcat
        |> commaSep
    where
        qualified field = if isEmpty (field.selectionSet)
                then tableName <> Snippet.sql "."
                else mempty

        compileSelection :: Selection -> [Snippet]
        compileSelection field@(Field {}) = [compileField field]
        compileSelection fragmentSpread@(FragmentSpread {}) = compileFragmentSpread fragmentSpread

        compileField :: Selection -> Snippet
        compileField field@(Field { alias = Just alias, name }) = qualified field <> sqlIdentifier (fieldNameToColumnName name) <> Snippet.sql " AS " <> sqlIdentifier alias
        compileField field@(Field { alias = Nothing, name    }) =
            let
                columnName = fieldNameToColumnName name
            in
                if columnName /= name
                    then qualified field <> sqlIdentifier (fieldNameToColumnName name) <> Snippet.sql " AS " <> sqlIdentifier name
                    else qualified field <> sqlIdentifier (fieldNameToColumnName name)


        compileFragmentSpread :: Selection -> [Snippet]
        compileFragmentSpread FragmentSpread { fragmentName } =
                fragment.selectionSet
                    |> map compileSelection
                    |> mconcat
            where
                fragment :: Fragment
                fragment = document.definitions
                    |> find (\case
                            FragmentDefinition (Fragment { name }) -> name == fragmentName
                            otherwise -> False
                        )
                    |> fromMaybe (error $ "Could not find fragment named " <> fragmentName)
                    |> \case
                        FragmentDefinition fragment -> fragment

fieldToJoin :: Document -> Text -> Selection -> Snippet
fieldToJoin document rootTableName field@(Field { name }) =
        Snippet.sql "LEFT JOIN LATERAL ("
            <> Snippet.sql "SELECT ARRAY("
                <> Snippet.sql "SELECT to_json(_sub) FROM ("
                    <> Snippet.sql "SELECT "
                    <> selectQueryPieces document foreignTable field
                    <> Snippet.sql " FROM " <> foreignTable
                    <> Snippet.sql " WHERE " <> foreignTable <> Snippet.sql "." <> foreignTableForeignKey <> Snippet.sql " = " <> rootTable <> Snippet.sql "." <> rootTablePrimaryKey
                <> Snippet.sql ") AS _sub"
            <> Snippet.sql ") AS " <> aliasOrName
        <> Snippet.sql ") " <> aliasOrName <> Snippet.sql " ON true"
    where
        foreignTable = sqlIdentifier name
        foreignTableForeignKey = sqlIdentifier foreignTableForeignKeyName
        foreignTableForeignKeyName = rootTableName
                |> singularize
                |> (\name -> name <> "_id")
        rootTable = sqlIdentifier rootTableName
        rootTablePrimaryKey = sqlIdentifier "id"

        aliasOrName = sqlIdentifier $
                case field.alias of
                    Just alias -> alias
                    Nothing -> field.name

compileMutationSelection :: [Argument] -> Selection -> Snippet
compileMutationSelection queryArguments field@(Field { alias, name = fieldName, arguments, selectionSet }) = fromMaybe (error ("Invalid mutation: " <> tshow fieldName)) do
        let create = do
                modelName <- Text.stripPrefix "create" fieldName
                pure $ compileSelectionToInsertStatement queryArguments field modelName

        let delete = do
                modelName <- Text.stripPrefix "delete" fieldName
                pure $ compileSelectionToDeleteStatement queryArguments field modelName

        let update = do
                modelName <- Text.stripPrefix "update" fieldName
                pure $ compileSelectionToUpdateStatement queryArguments field modelName

        create <|> delete <|> update

-- | Turns a @create..@ mutation into a INSERT SQL query
--
-- Input GraphQL document:
--
-- > mutation CreateProject($$project: Project) {
-- >     createProject(project: $$project) {
-- >         id title
-- >     }
-- > }
--
-- Input Arguments:
--
-- > project =
-- >     { title: "Hello World"
-- >     , userId: "dc984c2f-d91c-4143-9091-400ad2333f83"
-- >     }
--
-- Output SQL Query:
--
-- > INSERT INTO projects (user_id, title)
-- >     VALUES ('dc984c2f-d91c-4143-9091-400ad2333f83', 'Hello World')
-- >     RETURNING json_build_object('id', projects.id, 'title', projects.title)
--
compileSelectionToInsertStatement :: [Argument] -> Selection -> Text -> Snippet
compileSelectionToInsertStatement queryArguments field@(Field { alias, name = fieldName, arguments, selectionSet }) modelName =
        Snippet.sql "INSERT INTO " <> sqlIdentifier tableName <> Snippet.sql " (" <> commaSep columns <> Snippet.sql ") VALUES (" <> commaSep values <> Snippet.sql ") RETURNING " <> returning
    where
        tableName = modelNameToTableName modelName

        newRecord :: HashMap.HashMap Text Value
        newRecord = case headMay arguments of
            Just (Argument { argumentValue }) -> case resolveVariables argumentValue queryArguments of
                    ObjectValue hashMap -> hashMap
                    otherwise -> error $ "Expected first argument to " <> fieldName <> " to be an object, got: " <> tshow otherwise
            Nothing -> error $ "Expected first argument to " <> fieldName <> " to be an object, got no arguments"

        (columns, values) = newRecord
                |> HashMap.toList
                |> map (\(fieldName, value) -> (
                        sqlIdentifier (fieldNameToColumnName fieldName),
                        valueToSQL value
                    ))
                |> unzip

        returning :: Snippet
        returning = Snippet.sql "json_build_object(" <> returningArgs <> Snippet.sql ")"
        returningArgs = selectionSet
                |> map (\Field { name = fieldName } -> Snippet.param (fieldNameToColumnName fieldName) <> Snippet.sql ", " <> sqlIdentifier tableName <> Snippet.sql "." <> sqlIdentifier (fieldNameToColumnName fieldName))
                |> commaSep

-- | Turns a @update..@ mutation into a UPDATE SQL query
--
-- Input GraphQL document:
--
-- > mutation UpdateProject($projectId: ProjectId, patch: $patch) {
-- >     updateProject(id: $projectId, patch: $patch) {
-- >         id title
-- >     }
-- > }
--
-- Input Arguments:
--
-- > projectId = "df1f54d5-ced6-4f65-8aea-fcd5ea6b9df1"
-- > project =
-- >     { title: "Hello World"
-- >     , userId: "dc984c2f-d91c-4143-9091-400ad2333f83"
-- >     }
--
-- Output SQL Query:
--
-- > UPDATE projects
-- >     SET title = 'Hello World', user_id = 'dc984c2f-d91c-4143-9091-400ad2333f83'
-- >     WHERE id = 'df1f54d5-ced6-4f65-8aea-fcd5ea6b9df1'
-- >     RETURNING json_build_object('id', projects.id, 'title', projects.title)
--
compileSelectionToUpdateStatement :: [Argument] -> Selection -> Text -> Snippet
compileSelectionToUpdateStatement queryArguments field@(Field { alias, name = fieldName, arguments, selectionSet }) modelName =
        Snippet.sql "UPDATE " <> sqlIdentifier tableName <> Snippet.sql " SET " <> commaSep setValues <> where_ <> Snippet.sql " RETURNING " <> returning
    where
        tableName = modelNameToTableName modelName

        where_ = Snippet.sql " WHERE id = " <> recordId

        recordId = case headMay arguments of
            Just Argument { argumentValue } -> valueToSQL (resolveVariables argumentValue queryArguments)
            Nothing -> error $ "Expected first argument to " <> fieldName <> " to be an ID, got no arguments"

        patch :: HashMap.HashMap Text Value
        patch = case headMay <$> tail arguments of
            Just (Just (Argument { argumentValue })) -> case resolveVariables argumentValue queryArguments of
                    ObjectValue hashMap -> hashMap
                    otherwise -> error $ "Expected second argument to " <> fieldName <> " to be an object, got: " <> tshow otherwise
            _ -> error $ "Expected first argument to " <> fieldName <> " to be an object, got no arguments"

        setValues = patch
                |> HashMap.toList
                |> map (\(fieldName, value) -> sqlIdentifier (fieldNameToColumnName fieldName) <> Snippet.sql " = " <> valueToSQL value)

        returning :: Snippet
        returning = Snippet.sql "json_build_object(" <> returningArgs <> Snippet.sql ")"
        returningArgs = selectionSet
                |> map (\Field { name = fieldName } -> Snippet.param (fieldNameToColumnName fieldName) <> Snippet.sql ", " <> sqlIdentifier tableName <> Snippet.sql "." <> sqlIdentifier (fieldNameToColumnName fieldName))
                |> commaSep

-- | Turns a @delete..@ mutation into a DELETE SQL query
--
-- Input GraphQL document:
--
-- > mutation DeleteProject($$projectId: ProjectId) {
-- >     deleteProject(id: $$project) {
-- >         id title
-- >     }
-- > }
--
-- Input Arguments:
--
-- > projectId = "dc984c2f-d91c-4143-9091-400ad2333f83"
--
-- Output SQL Query:
--
-- > DELETE FROM projects
-- >     WHERE project_id = 'dc984c2f-d91c-4143-9091-400ad2333f83'
-- >     RETURNING json_build_object('id', projects.id, 'title', projects.title)
--
compileSelectionToDeleteStatement :: [Argument] -> Selection -> Text -> Snippet
compileSelectionToDeleteStatement queryArguments field@(Field { alias, name = fieldName, arguments, selectionSet }) modelName =
        Snippet.sql "DELETE FROM " <> sqlIdentifier tableName <> Snippet.sql " WHERE id = " <> recordId <> Snippet.sql " RETURNING " <> returning
    where
        tableName = modelNameToTableName modelName

        recordId = case headMay arguments of
            Just (Argument { argumentValue }) -> valueToSQL (resolveVariables argumentValue queryArguments)
            Nothing -> error $ "Expected first argument to " <> fieldName <> " to be an ID, got no arguments"

        returning :: Snippet
        returning = Snippet.sql "json_build_object(" <> returningArgs <> Snippet.sql ")"
        returningArgs = selectionSet
                |> map (\Field { name = fieldName } -> Snippet.param (fieldNameToColumnName fieldName) <> Snippet.sql ", " <> sqlIdentifier tableName <> Snippet.sql "." <> sqlIdentifier (fieldNameToColumnName fieldName))
                |> commaSep

valueToSQL :: Value -> Snippet
valueToSQL (IntValue int) = Snippet.param int
valueToSQL (StringValue string) = Snippet.param string


resolveVariables :: Value -> [Argument] -> Value
resolveVariables (Variable varName) arguments =
        arguments
        |> find (\Argument { argumentName } -> argumentName == varName)
        |> \case
            Just Argument { argumentValue } -> argumentValue
            Nothing -> error ("Could not resolve variable " <> varName)
resolveVariables otherwise _ = otherwise

unionAll :: [Snippet] -> Snippet
unionAll [] = mempty
unionAll list = mconcat $ List.intersperse (Snippet.sql " UNION ALL ") list

commaSep :: [Snippet] -> Snippet
commaSep [] = mempty
commaSep list = mconcat $ List.intersperse (Snippet.sql ", ") list

spaceSep :: [Snippet] -> Snippet
spaceSep [] = mempty
spaceSep list = mconcat $ List.intersperse (Snippet.sql " ") list
