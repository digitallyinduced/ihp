module IHP.GraphQL.Compiler where

import IHP.Prelude
import IHP.GraphQL.Types

import qualified Database.PostgreSQL.Simple.ToField as PG
import qualified Database.PostgreSQL.Simple.Types as PG
import Prelude (Semigroup (..))
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HashMap

data SqlQuery = SqlQuery { query :: Text, params :: [PG.Action]}

data QueryPart = QueryPart { sql :: PG.Query, params :: [PG.Action] }

compileDocument :: Variables -> Document -> [(PG.Query, [PG.Action])]
compileDocument (Variables arguments) document@(Document { definitions = (definition:rest) }) = 
    case definition of
        ExecutableDefinition { operation = OperationDefinition { operationType = Query } } ->
            [ unpackQueryPart ("SELECT to_json(_root.data) FROM (" <> compileDefinition document definition arguments <> ") AS _root") ]
        ExecutableDefinition { operation = OperationDefinition { operationType = Mutation } } ->
            map unpackQueryPart $ compileMutationDefinition definition arguments

compileDefinition :: Document -> Definition -> [Argument] -> QueryPart
compileDefinition document ExecutableDefinition { operation = OperationDefinition { operationType = Query, selectionSet } } variables =
    selectionSet
    |> map (compileSelection document variables)
    |> unionAll

compileMutationDefinition :: Definition -> [Argument] -> [QueryPart]
compileMutationDefinition ExecutableDefinition { operation = OperationDefinition { operationType = Mutation, selectionSet } } arguments =
    selectionSet
    |> map (compileMutationSelection arguments)

compileSelection :: Document -> [Argument] -> Selection -> QueryPart
compileSelection document variables field@(Field { alias, name = fieldName, arguments }) = 
        ("(SELECT json_build_object(?, json_agg(?.*)) AS data FROM (SELECT " |> withParams [PG.toField nameOrAlias, PG.toField (PG.Identifier subqueryId)])
        <> selectQueryPieces document (PG.toField (PG.Identifier tableName)) field
        <> (" FROM ?" |> withParams [PG.toField (PG.Identifier tableName)])
        <> joins
        <> where_
        <> (") AS ?)" |> withParams [ PG.toField (PG.Identifier subqueryId) ])
    where
        subqueryId = "_" <> fieldName
        nameOrAlias = fromMaybe fieldName alias

        tableName = if isJust idArgument
            then pluralize fieldName
            else fieldName

        where_ :: QueryPart
        where_ = case idArgument of
                Just id -> " WHERE id = ?" |> withParams [valueToSQL $ resolveVariables id variables]
                Nothing -> ""

        idArgument :: Maybe Value
        idArgument = case arguments of
                [Argument { argumentName = "id", argumentValue }] -> Just argumentValue
                _ -> Nothing


        isJoinField :: Selection -> Bool
        isJoinField Field { selectionSet } = not (null selectionSet)
        isJoinField FragmentSpread {} = False -- TODO: Also support fragment spreads in joined tables

        joins :: QueryPart
        joins = field
                |> get #selectionSet
                |> filter isJoinField
                |> map (fieldToJoin document tableName)
                |> \case
                    [] -> ""
                    joins -> " " <> spaceSep joins


selectQueryPieces :: Document -> PG.Action -> Selection -> QueryPart
selectQueryPieces document tableName field = field
        |> get #selectionSet
        |> map compileSelection
        |> mconcat
        |> commaSep
    where
        qualified field = if isEmpty (get #selectionSet field)
                then "?." |> withParams [tableName]
                else ""

        compileSelection :: Selection -> [QueryPart]
        compileSelection field@(Field {}) = [compileField field]
        compileSelection fragmentSpread@(FragmentSpread {}) = compileFragmentSpread fragmentSpread

        compileField :: Selection -> QueryPart
        compileField field@(Field { alias = Just alias, name }) = qualified field <> "? AS ?" |> withParams [ PG.toField (PG.Identifier (fieldNameToColumnName name)), PG.toField (PG.Identifier alias) ]
        compileField field@(Field { alias = Nothing, name    }) =
            let
                columnName = fieldNameToColumnName name
            in
                if columnName /= name
                    then qualified field <> "? AS ?" |> withParams [ PG.toField (PG.Identifier (fieldNameToColumnName name)), PG.toField (PG.Identifier name) ]
                    else qualified field <> "?" |> withParams [ PG.toField (PG.Identifier (fieldNameToColumnName name)) ]

        
        compileFragmentSpread :: Selection -> [QueryPart]
        compileFragmentSpread FragmentSpread { fragmentName } = 
                fragment
                    |> get #selectionSet
                    |> map compileSelection
                    |> mconcat
            where
                fragment :: Fragment
                fragment = document
                    |> get #definitions
                    |> find (\case
                            FragmentDefinition (Fragment { name }) -> name == fragmentName
                            otherwise -> False
                        )
                    |> fromMaybe (error $ "Could not find fragment named " <> fragmentName)
                    |> \case
                        FragmentDefinition fragment -> fragment

fieldToJoin :: Document -> Text -> Selection -> QueryPart
fieldToJoin document rootTableName field@(Field { name }) =
        "LEFT JOIN LATERAL ("
            <> "SELECT ARRAY("
                <> "SELECT to_json(_sub) FROM ("
                    <> "SELECT "
                    <> selectQueryPieces document foreignTable field
                    <> (" FROM ?" |> withParams [foreignTable])
                    <> (" WHERE ?.? = ?.?" |> withParams [foreignTable, foreignTableForeignKey, rootTable, rootTablePrimaryKey])
                <> ") AS _sub"
            <> (") AS ?" |> withParams [aliasOrName])
        <> (") ? ON true" |> withParams [aliasOrName])
    where
        foreignTable = PG.toField (PG.Identifier name)
        foreignTableForeignKey = PG.toField (PG.Identifier foreignTableForeignKeyName)
        foreignTableForeignKeyName = rootTableName
                |> singularize
                |> (\name -> name <> "_id")
        rootTable = PG.toField (PG.Identifier rootTableName)
        rootTablePrimaryKey = PG.toField (PG.Identifier "id")

        aliasOrName = PG.toField $ PG.Identifier $
                case get #alias field of
                    Just alias -> alias
                    Nothing -> get #name field

compileMutationSelection :: [Argument] -> Selection -> QueryPart
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
compileSelectionToInsertStatement :: [Argument] -> Selection -> Text -> QueryPart
compileSelectionToInsertStatement queryArguments field@(Field { alias, name = fieldName, arguments, selectionSet }) modelName =
        ("INSERT INTO ? (" |> withParams [PG.toField $ PG.Identifier tableName]) <> commaSep columns <> ") VALUES (" <> commaSep values <> ") RETURNING " <> returning
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
                        ("?" |> withParams [PG.toField (PG.Identifier (fieldNameToColumnName fieldName))]),
                        ("?" |> withParams [valueToSQL value])
                    ))
                |> unzip

        returning :: QueryPart
        returning = "json_build_object(" <> returningArgs <> ")"
        returningArgs = selectionSet
                |> map (\Field { name = fieldName } -> "?, ?.?" |> withParams [PG.toField (fieldNameToColumnName fieldName), PG.toField (PG.Identifier tableName), PG.toField (PG.Identifier (fieldNameToColumnName fieldName))])
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
compileSelectionToUpdateStatement :: [Argument] -> Selection -> Text -> QueryPart
compileSelectionToUpdateStatement queryArguments field@(Field { alias, name = fieldName, arguments, selectionSet }) modelName =
        ("UPDATE ? SET " |> withParams [PG.toField $ PG.Identifier tableName]) <> commaSep setValues <> where_ <> " RETURNING " <> returning
    where
        tableName = modelNameToTableName modelName

        where_ = " WHERE id = ?" |> withParams [recordId]

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
                |> map (\(fieldName, value) -> ("? = ?" |> withParams [PG.toField (PG.Identifier (fieldNameToColumnName fieldName)), valueToSQL value]))

        returning :: QueryPart
        returning = "json_build_object(" <> returningArgs <> ")"
        returningArgs = selectionSet
                |> map (\Field { name = fieldName } -> "?, ?.?" |> withParams [PG.toField (fieldNameToColumnName fieldName), PG.toField (PG.Identifier tableName), PG.toField (PG.Identifier (fieldNameToColumnName fieldName))])
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
compileSelectionToDeleteStatement :: [Argument] -> Selection -> Text -> QueryPart
compileSelectionToDeleteStatement queryArguments field@(Field { alias, name = fieldName, arguments, selectionSet }) modelName =
        ("DELETE FROM ? WHERE id = ?" |> withParams [PG.toField $ PG.Identifier tableName, recordId]) <> " RETURNING " <> returning
    where
        tableName = modelNameToTableName modelName

        recordId = case headMay arguments of
            Just (Argument { argumentValue }) -> valueToSQL (resolveVariables argumentValue queryArguments)
            Nothing -> error $ "Expected first argument to " <> fieldName <> " to be an ID, got no arguments"
        
        returning :: QueryPart
        returning = "json_build_object(" <> returningArgs <> ")"
        returningArgs = selectionSet
                |> map (\Field { name = fieldName } -> "?, ?.?" |> withParams [PG.toField (fieldNameToColumnName fieldName), PG.toField (PG.Identifier tableName), PG.toField (PG.Identifier (fieldNameToColumnName fieldName))])
                |> commaSep

valueToSQL :: Value -> PG.Action
valueToSQL (IntValue int) = PG.toField int
valueToSQL (StringValue string) = PG.toField string


resolveVariables :: Value -> [Argument] -> Value
resolveVariables (Variable varName) arguments =
        arguments
        |> find (\Argument { argumentName } -> argumentName == varName)
        |> \case
            Just Argument { argumentValue } -> argumentValue
            Nothing -> error ("Could not resolve variable " <> varName)
resolveVariables otherwise _ = otherwise

unionAll :: [QueryPart] -> QueryPart
unionAll list = foldl' (\a b -> if get #sql a == "" then b else a <> " UNION ALL " <> b) "" list

commaSep :: [QueryPart] -> QueryPart
commaSep list = foldl' (\a b -> if get #sql a == "" then b else a <> ", " <> b) "" list

spaceSep :: [QueryPart] -> QueryPart
spaceSep list = foldl' (\a b -> if get #sql a == "" then b else a <> " " <> b) "" list

instance Semigroup QueryPart where
    QueryPart { sql = sqlA, params = paramsA } <> QueryPart { sql = sqlB, params = paramsB } = QueryPart { sql = sqlA <> sqlB, params = paramsA <> paramsB }
instance Monoid QueryPart where
    mempty = QueryPart { sql = "", params = [] }

instance IsString QueryPart where
    fromString string = QueryPart { sql = fromString string, params = [] }

unpackQueryPart :: QueryPart -> (PG.Query, [PG.Action])
unpackQueryPart QueryPart { sql, params } = (sql, params)

withParams :: [PG.Action] -> QueryPart -> QueryPart
withParams params queryPart = queryPart { params = (get #params queryPart) <> params }