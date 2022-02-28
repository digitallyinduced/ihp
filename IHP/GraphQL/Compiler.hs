module IHP.GraphQL.Compiler where

import IHP.Prelude
import IHP.GraphQL.Types

import qualified Database.PostgreSQL.Simple.ToField as PG
import qualified Database.PostgreSQL.Simple.Types as PG
import Prelude (Semigroup (..))

data SqlQuery = SqlQuery { query :: Text, params :: [PG.Action]}

data QueryPart = QueryPart { sql :: PG.Query, params :: [PG.Action] }

compileDocument :: Document -> (PG.Query, [PG.Action])
compileDocument Document { definitions = [definition] } = unpackQueryPart $ "SELECT json_agg(_root.data) FROM (" <> compileDefinition definition <> ") AS _root"

compileDefinition :: Definition -> QueryPart
compileDefinition ExecutableDefinition { operation = OperationDefinition { selectionSet } } =
    selectionSet
    |> map compileSelection
    |> unionAll

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