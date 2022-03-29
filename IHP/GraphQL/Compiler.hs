module IHP.GraphQL.Compiler where

import IHP.Prelude
import IHP.GraphQL.Types
import qualified IHP.GraphQL.Introspection as Introspection

import qualified Database.PostgreSQL.Simple.ToField as PG
import qualified Database.PostgreSQL.Simple.Types as PG
import Prelude (Semigroup (..))
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List

data SqlQuery = SqlQuery { query :: Text, params :: [PG.Action]}

data QueryPart = QueryPart { sql :: PG.Query, params :: [PG.Action] }

compileDocument :: Variables -> Document -> [(PG.Query, [PG.Action])]
compileDocument (Variables arguments) document@(Document { definitions = (definition:rest) }) = 
    case definition of
        ExecutableDefinition { operation = OperationDefinition { operationType } } | operationType == Query || operationType == Subscription ->
            [ unpackQueryPart (compileDefinition document definition arguments) ]
        ExecutableDefinition { operation = OperationDefinition { operationType = Mutation } } ->
            map unpackQueryPart $ compileMutationDefinition document definition arguments

compileDefinition :: Document -> Definition -> [Argument] -> QueryPart
compileDefinition document ExecutableDefinition { operation = OperationDefinition { operationType, selectionSet } } variables | operationType == Query || operationType == Subscription =
    "SELECT json_build_object(" <> commaSep aggregations <> ")"
    where
        aggregations = map (compileSelection document variables) selectionSet

compileMutationDefinition :: Document -> Definition -> [Argument] -> [QueryPart]
compileMutationDefinition document ExecutableDefinition { operation = OperationDefinition { operationType = Mutation, selectionSet } } arguments =
    selectionSet
    |> map ((compileMutationSelection document) arguments)

compileSelection :: Document -> [Argument] -> Selection -> QueryPart
compileSelection document variables field@(Field { alias, name = fieldName, arguments }) = 
        aggregation
    where
        query =
            "(SELECT "
            <> selectQueryPieces document tableName field
            <> (" FROM ?" |> withParams [PG.toField (PG.Identifier tableName)])
            <> joins document variables tableName field
            <> where_
            <> (") AS ?" |> withParams [ PG.toField (PG.Identifier subqueryId) ])

        -- | Builds a tuple as used in `json_build_object('users', json_agg(_users), 'tasks', json_agg(_tasks))`
        aggregation = (
                if isSingleResult
                    then
                        (("?, (SELECT coalesce(row_to_json(?), '[]'::json) FROM " |> withParams [PG.toField nameOrAlias, PG.toField (PG.Identifier subqueryId)]) <> query <> ")")
                    else
                        (("?, (SELECT coalesce(json_agg(row_to_json(?)), '[]'::json) FROM " |> withParams [PG.toField nameOrAlias, PG.toField (PG.Identifier subqueryId)]) <> query <> ")"))

        isSingleResult = isJust idArgument

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

joins :: Document -> [Argument] -> Text -> Selection -> QueryPart
joins document variables tableName field = field
        |> get #selectionSet
        |> filter isJoinField
        |> map (fieldToJoin document variables tableName)
        |> \case
            [] -> ""
            joins -> " " <> spaceSep joins
    where
        isJoinField :: Selection -> Bool
        isJoinField Field { selectionSet } = not (null selectionSet)
        isJoinField FragmentSpread {} = False -- TODO: Also support fragment spreads in joined tables



selectQueryPieces :: Document -> Text -> Selection -> QueryPart
selectQueryPieces document tableName field =
    selectFields document tableName field
    |> map (\(left, right, isAlias) -> if isAlias
            then left <> " AS ?" |> withParams [PG.toField (PG.Identifier right)]
            else left
        )
    |> commaSep

returnQueryPieces :: Document -> Text -> Selection -> QueryPart
returnQueryPieces document tableName field =
    selectFields document tableName field
    |> map (\(left, right, isAlias) -> ("?, " |> withParams [PG.toField right]) <> left )
    |> commaSep

selectFields :: Document -> Text -> Selection -> [(QueryPart, Text, Bool)]
selectFields document tableName field =
        field
        |> get #selectionSet
        |> map compileSelection
        |> mconcat
    where
        qualified :: Selection -> QueryPart
        qualified field = if isEmpty (get #selectionSet field)
                then "?." |> withParams [PG.toField (PG.Identifier tableName)]
                else ""

        compileSelection :: Selection -> [(QueryPart, Text, Bool)]
        compileSelection field@(Field {}) = [compileField field]
        compileSelection fragmentSpread@(FragmentSpread {}) = compileFragmentSpread fragmentSpread

        compileField :: Selection -> (QueryPart, Text, Bool)
        compileField field@(Field { alias, name = "__typename" }) =
                ( "?" |> withParams [ PG.toField typeName ]
                , fromMaybe "__typename" alias
                , alias /= "__typename"
                )
            where
                typeName = tableNameToModelName tableName
        compileField field@(Field { alias = Just alias, name }) =
                ( qualified field <> ("?" |> withParams [ PG.toField (PG.Identifier (fieldNameToColumnName name)) ])
                , alias
                , True
                )
        compileField field@(Field { alias = Nothing, name }) =
            let
                columnName = fieldNameToColumnName name
            in
                ( qualified field <> ("?" |> withParams [ PG.toField (PG.Identifier (fieldNameToColumnName name)) ])
                , name
                , columnName /= name
                )

        
        compileFragmentSpread :: Selection -> [(QueryPart, Text, Bool)]
        compileFragmentSpread FragmentSpread { fragmentName } = 
                fragment
                    |> get #selectionSet
                    |> map compileSelection
                    |> mconcat
            where
                fragment = findFragmentByName document fragmentName

fieldToJoin :: Document -> [Argument] -> Text -> Selection -> QueryPart
fieldToJoin document variables rootTableName field@(Field { name }) =
        "LEFT JOIN LATERAL ("
            <> when isHasMany "SELECT ARRAY("
                <> when isHasMany "SELECT to_json(_sub) FROM ("
                    <> "SELECT "
                    <> selectQueryPieces document foreignTableName field
                    <> (" FROM ?" |> withParams [foreignTable])
                    <> joins document variables rootTableName field
                    <> (" WHERE ?.? = ?.?" |> withParams conditionParams)
                <> when isHasMany ") AS _sub"
            <> when isHasMany (") AS ?" |> withParams [aliasOrName])
        <> (") ? ON true" |> withParams [aliasOrName])
    where
        isHasOne :: Bool
        isHasOne = singularize name == name -- Is it a singular name, like `user` instead of `users`?

        isHasMany = not isHasOne

        conditionParams = if isHasMany
            then [foreignTable, foreignTableForeignKey, rootTable, rootTablePrimaryKey]
            else [foreignTable, PG.toField (PG.Identifier "id"), rootTable, PG.toField (PG.Identifier $ (fieldNameToColumnName name) <> "_id" )]

        when condition then_ = if condition then then_ else ""

        foreignTable = PG.toField (PG.Identifier foreignTableName)
        foreignTableName =
            if isHasOne
                then pluralize name
                else name

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

compileMutationSelection :: Document -> [Argument] -> Selection -> QueryPart
compileMutationSelection document queryArguments field@(Field { alias, name = fieldName, arguments, selectionSet }) = fromMaybe (error ("Invalid mutation: " <> tshow fieldName)) do
        let create = do
                modelName <- Text.stripPrefix "create" fieldName
                pure $ compileSelectionToInsertStatement document queryArguments field modelName

        let delete = do
                modelName <- Text.stripPrefix "delete" fieldName
                pure $ compileSelectionToDeleteStatement document queryArguments field modelName
        
        let update = do
                modelName <- Text.stripPrefix "update" fieldName
                pure $ compileSelectionToUpdateStatement document queryArguments field modelName

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
compileSelectionToInsertStatement :: Document -> [Argument] -> Selection -> Text -> QueryPart
compileSelectionToInsertStatement document queryArguments field@(Field { alias, name = fieldName, arguments, selectionSet }) modelName =
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
        returning = ("json_build_object(?, json_build_object(" |> withParams [PG.toField (nameOrAlias field) ]) <> returnQueryPieces document tableName field <> "))"

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
compileSelectionToUpdateStatement :: Document -> [Argument] -> Selection -> Text -> QueryPart
compileSelectionToUpdateStatement document queryArguments field@(Field { alias, name = fieldName, arguments, selectionSet }) modelName =
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
        returning = ("json_build_object(?, json_build_object(" |> withParams [PG.toField (nameOrAlias field) ]) <> returnQueryPieces document tableName field <> "))"

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
compileSelectionToDeleteStatement :: Document -> [Argument] -> Selection -> Text -> QueryPart
compileSelectionToDeleteStatement document queryArguments field@(Field { alias, name = fieldName, arguments, selectionSet }) modelName =
        ("DELETE FROM ? WHERE id = ?" |> withParams [PG.toField $ PG.Identifier tableName, recordId]) <> " RETURNING " <> returning
    where
        tableName = modelNameToTableName modelName

        recordId = case headMay arguments of
            Just (Argument { argumentValue }) -> valueToSQL (resolveVariables argumentValue queryArguments)
            Nothing -> error $ "Expected first argument to " <> fieldName <> " to be an ID, got no arguments"
        
        returning :: QueryPart
        returning = ("json_build_object(?, json_build_object(" |> withParams [PG.toField (nameOrAlias field) ]) <> returnQueryPieces document tableName field <> "))"

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

compileIntrospectionSelection :: GraphQLSchema -> Document -> [Argument] -> Selection -> (Maybe QueryPart, QueryPart)
compileIntrospectionSelection schema document variables field@(Field { name, selectionSet }) = (Nothing, aggregation)
    where

        aggregation = ("?, json_build_object(" |> withParams [PG.toField (nameOrAlias field)]) <> buildSchemaSelection <> ")"

        buildSchemaSelection = commaSep (map (compileSchemaSelection (Introspection.introspectionGraph schema)) selectionSet)

        compileSchemaSelection :: StaticGraph -> Selection -> QueryPart
        compileSchemaSelection graph field@(Field { name, selectionSet = [] }) =
            let
                targetLeaf :: Value
                targetLeaf = graph
                        |> (\case
                                ObjectNode { objectValues } -> objectValues
                                otherwise -> error $ "expected object node, got " <> tshow otherwise
                            )
                        |> HashMap.lookup name
                        |> \case
                            Just (Leaf value) -> value
                            otherwise -> error $ "expected leaf node at " <> name <> ", got " <> tshow otherwise <> " in graph " <> tshow graph
            in
                "?, ?" |> withParams [PG.toField (nameOrAlias field), PG.toField targetLeaf]
        compileSchemaSelection graph field@(Field { name, selectionSet }) =
            let
                targetNode :: StaticGraph
                targetNode = graph
                        |> (\case
                                ObjectNode { objectValues } -> objectValues
                                otherwise -> error $ "expected object node, got " <> tshow otherwise
                            )
                        |> HashMap.lookup name
                        |> fromMaybe (error $ "Could not find node " <> name)
            in
                case targetNode of
                    ObjectNode {} -> ("?, json_build_object(" |> withParams [PG.toField (nameOrAlias field)]) <> commaSep (map (compileSchemaSelection targetNode) selectionSet) <> ")"
                    ArrayNode { arrayElements } -> ("?, json_build_object(" |> withParams [PG.toField (nameOrAlias field)]) <> commaSep (map (\targetNode -> commaSep (map (compileSchemaSelection targetNode) selectionSet)) arrayElements) <> ")"
                    Leaf { value = NullValue } -> "?, null" |> withParams [PG.toField (nameOrAlias field)]
                    otherwise -> error $ "Expected object or array, got " <> tshow otherwise <> " while trying to access " <> name
        compileSchemaSelection graph FragmentSpread { fragmentName } =
            let
                fragment = findFragmentByName document fragmentName
                selectionSet = get #selectionSet fragment
            in
                commaSep (map (compileSchemaSelection graph) selectionSet)


unionAll :: [QueryPart] -> QueryPart
unionAll list = foldl' (\a b -> if get #sql a == "" then b else a <> " UNION ALL " <> b) "" list

commaSep :: [QueryPart] -> QueryPart
commaSep list = foldl' (\a b -> if get #sql a == "" then b else (a <> ", " <> b)) "" list

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

nameOrAlias :: Selection -> Text
nameOrAlias field = fromMaybe (get #name field) (get #alias field)

findFragmentByName :: Document -> Text -> Fragment
findFragmentByName document name =
    let
        allFragmentNames = document
                |> get #definitions
                |> mapMaybe (\case FragmentDefinition (Fragment { name }) -> Just name; _ -> Nothing)
        couldNotFindFragmentErrorMessage = "Could not find fragment named " <> name <> ". These fragments are defined: " <> Text.intercalate ", " allFragmentNames
    in
        document
            |> get #definitions
            |> find (\case
                    FragmentDefinition (Fragment { name = fragmentName }) -> name == fragmentName
                    otherwise -> False
                )
            |> fromMaybe (error couldNotFindFragmentErrorMessage)
            |> \case
                FragmentDefinition fragment -> fragment

instance PG.ToField Value where
    toField (StringValue string) = PG.toField string
    toField NullValue = PG.toField (Nothing :: Maybe Int)