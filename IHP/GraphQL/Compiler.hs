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

compileDocument :: [Argument] -> Document -> [(PG.Query, [PG.Action])]
compileDocument arguments Document { definitions = [definition] } = 
    case definition of
        ExecutableDefinition { operation = OperationDefinition { operationType = Query } } ->
            [ unpackQueryPart ("SELECT json_agg(_root.data) FROM (" <> compileDefinition definition <> ") AS _root") ]
        ExecutableDefinition { operation = OperationDefinition { operationType = Mutation } } ->
            map unpackQueryPart $ compileMutationDefinition definition arguments

compileDefinition :: Definition -> QueryPart
compileDefinition ExecutableDefinition { operation = OperationDefinition { operationType = Query, selectionSet } } =
    selectionSet
    |> map compileSelection
    |> unionAll

compileMutationDefinition :: Definition -> [Argument] -> [QueryPart]
compileMutationDefinition ExecutableDefinition { operation = OperationDefinition { operationType = Mutation, selectionSet } } arguments =
    selectionSet
    |> map (compileSelectionToInsertStatement arguments)

compileSelection :: Selection -> QueryPart
compileSelection field@(Field { alias, name = fieldName }) = 
        ("(SELECT json_build_object(?, json_agg(?.*)) AS data FROM (SELECT " |> withParams [PG.toField nameOrAlias, PG.toField (PG.Identifier subqueryId)])
        <> selectQueryPieces
        <> (" FROM ?) AS ?)" |> withParams [ PG.toField (PG.Identifier fieldName), PG.toField (PG.Identifier subqueryId) ])
    where
        subqueryId = "_" <> fieldName
        nameOrAlias = fromMaybe fieldName alias

        selectQueryPieces = field
                |> get #selectionSet
                |> map compileField
                |> commaSep

        compileField :: Selection -> QueryPart
        compileField Field { alias = Just alias, name } = "? AS ?" |> withParams [ PG.toField (PG.Identifier name), PG.toField (PG.Identifier alias) ]
        compileField Field { alias = Nothing, name    } = "?" |> withParams [ PG.toField (PG.Identifier name) ]

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
-- > { title: "Hello World"
-- > , userId: "dc984c2f-d91c-4143-9091-400ad2333f83"
-- > }
--
-- Output SQL Query:
--
-- > INSERT INTO projects (user_id, title)
-- >     VALUES ('dc984c2f-d91c-4143-9091-400ad2333f83', 'Hello World')
-- >     RETURNING json_build_object('id', projects.id, 'title', projects.title)
--
compileSelectionToInsertStatement :: [Argument] -> Selection -> QueryPart
compileSelectionToInsertStatement queryArguments field@(Field { alias, name = fieldName, arguments, selectionSet }) =
        ("INSERT INTO ? (" |> withParams [PG.toField $ PG.Identifier tableName]) <> commaSep columns <> ") VALUES (" <> commaSep values <> ") RETURNING " <> returning
    where
        (Just modelName) = Text.stripPrefix "create" fieldName
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
        
        valueToSQL :: Value -> PG.Action
        valueToSQL (IntValue int) = PG.toField int
        valueToSQL (StringValue string) = PG.toField string

        returning :: QueryPart
        returning = "json_build_object(" <> returningArgs <> ")"
        returningArgs = selectionSet
                |> map (\Field { name = fieldName } -> "?, ?.?" |> withParams [PG.toField (fieldNameToColumnName fieldName), PG.toField (PG.Identifier tableName), PG.toField (PG.Identifier (fieldNameToColumnName fieldName))])
                |> commaSep

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