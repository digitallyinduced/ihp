{-|
Module: IHP.TypedSql.Pagination
Description: Add pagination to typedSql queries
Copyright: (c) digitally induced GmbH, 2025

This is the @typedSql@ analogue of the raw-SQL paginators in
"IHP.Pagination.ControllerFunctions" ('paginatedSqlQuery' /
'paginatedSqlQueryWithOptions'). It lives in @ihp-typed-sql@ because that is the
only package that can see both 'TypedQuery' (defined here) and the 'Pagination'
\/ 'Options' types (defined in the @ihp@ package), without introducing a
dependency cycle — @ihp-typed-sql@ already depends on @ihp@.
-}
module IHP.TypedSql.Pagination
( paginatedTypedSql
, paginatedTypedSqlWithOptions
) where

import IHP.Prelude
import IHP.Pagination.Types (Options(..), Pagination(..))
import IHP.Pagination.ControllerFunctions (defaultPaginationOptions)
import IHP.Pagination.Internal (pageSize', page, offset')
import IHP.ModelSupport (sqlQueryHasql)
import IHP.Hasql.Encoders () -- For the 'DefaultParamEncoder Int' instance used by 'Snippet.param'
import IHP.TypedSql.Types (TypedQuery(..))
import Network.Wai (Request)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.DynamicStatements.Snippet as Snippet

-- | Runs a 'TypedQuery' and adds pagination to it.
--
-- This is the @typedSql@ counterpart to 'paginatedSqlQuery'. By default, the
-- pagination uses the following options:
--
-- 1. Maximum items per page: 50. Each page will show at most 50 items.
-- 2. Selector window size: 5. The selector will show the current page, and 5 pages before and after it,
--    if they exist.
--
-- This function should be used inside your controller action. It will do three things:
--
--     1. Using the 'page' (current page number to display) and 'maxItems' (which overrides the set maximum
--        items per page) request parameters, this applies the the needed limit and offset to display the
--        correct page. For instance, page 3 with a maxItems of 50 would produce a limit of 50 and an offset
--        of 100 to display results 100 through 150.
--     2. Returns a 'Pagination' state which should be passed through to your view and then,
--        in turn, 'renderPagination'.
--     3. Actually run the query and return the result.
--
-- __Example:__
--
-- > (users, pagination) <- paginatedTypedSql
-- >     [typedSql| SELECT id, firstname, lastname FROM users ORDER BY lastname |]
--
-- The query is wrapped in a subquery (@SELECT subquery.* FROM (<your query>) as subquery@)
-- before @LIMIT@ \/ @OFFSET@ are applied, so any @ORDER BY@ must live __inside__ the query you
-- pass in (as in the example above), exactly like with 'paginatedSqlQuery'.
--
-- *AutoRefresh:* When using 'paginatedTypedSql' with AutoRefresh, you need to use 'trackTableRead' to let AutoRefresh know that you have accessed a certain table. Otherwise AutoRefresh will not watch the table of your custom sql query.
paginatedTypedSql
    :: ( ?request :: Request
       , ?modelContext :: ModelContext
       )
    => TypedQuery model
    -> IO ([model], Pagination)
paginatedTypedSql = paginatedTypedSqlWithOptions defaultPaginationOptions

-- | Runs a 'TypedQuery' and adds pagination to it.
--
-- This function accepts the same 'Options' as 'paginateWithOptions', but otherwise behaves like 'paginatedTypedSql'.
--
-- __Example:__
--
-- > (users, pagination) <- paginatedTypedSqlWithOptions
-- >     (defaultPaginationOptions |> set #maxItems 10)
-- >     [typedSql| SELECT id, firstname, lastname FROM users ORDER BY lastname |]
--
-- The query is wrapped in a subquery (@SELECT subquery.* FROM (<your query>) as subquery@)
-- before @LIMIT@ \/ @OFFSET@ are applied, so any @ORDER BY@ must live __inside__ the query you
-- pass in (as in the example above), exactly like with 'paginatedSqlQueryWithOptions'.
--
-- *AutoRefresh:* When using 'paginatedTypedSqlWithOptions' with AutoRefresh, you need to use 'trackTableRead' to let AutoRefresh know that you have accessed a certain table. Otherwise AutoRefresh will not watch the table of your custom sql query.
paginatedTypedSqlWithOptions
    :: ( ?request :: Request
       , ?modelContext :: ModelContext
       )
    => Options
    -> TypedQuery model
    -> IO ([model], Pagination)
paginatedTypedSqlWithOptions options TypedQuery { tqSnippet, tqResultDecoder } = do
    let pool = ?modelContext.hasqlPool

    let countSnippet = Snippet.sql "SELECT count(subquery.*) FROM (" <> tqSnippet <> Snippet.sql ") as subquery"
    count :: Int <- sqlQueryHasql pool countSnippet (Decoders.singleRow (Decoders.column (Decoders.nonNullable (fromIntegral <$> Decoders.int8))))

    let pageSize = pageSize' options
        pagination = Pagination
            { pageSize = pageSize
            , totalItems = fromIntegral count
            , currentPage = fromIntegral page
            , window = windowSize options
            }

    let resultsSnippet =
            Snippet.sql "SELECT subquery.* FROM ("
            <> tqSnippet
            <> Snippet.sql ") as subquery LIMIT "
            <> Snippet.param pageSize
            <> Snippet.sql " OFFSET "
            <> Snippet.param (offset' pageSize page)
    results <- sqlQueryHasql pool resultsSnippet (Decoders.rowList tqResultDecoder)

    pure (results, pagination)
