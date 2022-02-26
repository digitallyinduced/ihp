module IHP.GraphQL.Compiler where

import IHP.Prelude
import IHP.GraphQL.Types

import qualified Database.PostgreSQL.Simple.ToField as PG
import qualified Database.PostgreSQL.Simple.Types as PG

data SqlQuery = SqlQuery { query :: Text, params :: [PG.Action]}

compileDocument :: Document -> (PG.Query, [PG.Action])
compileDocument Document { definitions } = ("SELECT json_agg(_root.data) FROM (" <> unionAll definitionsQueries <> ") AS _root", concat definitionsParams)
    where
        (definitionsQueries, definitionsParams) = unzip $ map compileDefinition definitions

compileDefinition :: Definition -> (PG.Query, [PG.Action])
compileDefinition ExecutableDefinition { operation = OperationDefinition { selectionSet = [Field { name = fieldName } ] } } =
    ("(SELECT json_build_object(?, json_agg(?.*)) AS data FROM (SELECT * FROM ?) AS ?)", [PG.toField fieldName, PG.toField (PG.Identifier subqueryId), PG.toField (PG.Identifier fieldName),  PG.toField (PG.Identifier subqueryId)])
    where
        subqueryId = "_" <> fieldName

unionAll :: [PG.Query] -> PG.Query
unionAll list = foldl' (\a b -> if a == "" then b else a <> " UNION ALL " <> b) "" list