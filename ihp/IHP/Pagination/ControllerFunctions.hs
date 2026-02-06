{-|
Module: IHP.Pagination.ControllerFunctions
Description: Paginate results in your actions
Copyright: (c) digitally induced GmbH, 2021
-}
module IHP.Pagination.ControllerFunctions
(   paginate,
    paginateWithOptions,
    filterList,
    defaultPaginationOptions,
    paginatedSqlQuery,
    paginatedSqlQueryWithOptions,
) where

import IHP.Prelude
import IHP.Controller.Context
import IHP.Controller.Param ( paramOrDefault, paramOrNothing )
import IHP.Pagination.Types ( Options(..), Pagination(..) )
import IHP.QueryBuilder ( HasQueryBuilder, filterWhereILike, limit, offset )
import IHP.Fetch (fetchCount)
import IHP.ModelSupport (GetModelByTableName, sqlQuery, sqlQueryScalar, Table)
import qualified Network.Wai as Wai

import qualified Hasql.DynamicStatements.Snippet as Snippet
import qualified Hasql.Decoders as Decoders

-- | Paginate a query, with the following default options:
--
-- 1. Maximum items per page: 50. Each page will show at most 50 items.
-- 2. Selector window size: 5. The selector will show the current page, and 5 pages before and after it,
--    if they exist.
--
-- This function should be used inside your controller action. It will do two things:
--
--     1. Using the 'page' (current page number to display) and 'maxItems' (which overrides the set maximum
--        items per page) request parameters, this applies the the needed limit and offset to display the
--        correct page. For instance, page 3 with a maxItems of 50 would produce a limit of 50 and an offset
--        of 100 to display results 100 through 150.
--     2. Returns a 'Pagination' state which should be passed through to your view and then,
--        in turn, 'renderPagination'.
--
-- Example:
--
-- > action UsersAction = do
-- >    (userQ, pagination) <- query @User
-- >        |> orderBy #email
-- >        |> paginate
-- >    user <- userQ |> fetch
-- >    render IndexView { .. }
paginate :: forall controller table queryBuilderProvider joinRegister .
    (?context::ControllerContext
    , ?modelContext :: ModelContext
    , ?theAction :: controller
    , ?request :: Wai.Request
    , KnownSymbol table
    , HasQueryBuilder queryBuilderProvider joinRegister) =>
    queryBuilderProvider table
    -> IO (queryBuilderProvider table, Pagination)
paginate = paginateWithOptions defaultPaginationOptions

-- | Paginate with ability to override the default options for maximum items per page and selector window size.
--
-- This function should be used inside your controller action. It will do two things:
--
--     1. Using the 'page' (current page number to display) and 'maxItems' (which overrides the set maximum
--        items per page) request parameters, this applies the the needed limit and offset to display the
--        correct page. For instance, page 3 with a maxItems of 50 would produce a limit of 50 and an offset
--        of 100 to display results 100 through 150.
--     2. Returns a 'Pagination' state which should be passed through to your view and then,
--        in turn, 'renderPagination'.
--
-- Example:
--
-- > action UsersAction = do
-- >    (userQ, pagination) <- query @User
-- >        |> orderBy #email
-- >        |> paginateWithOptions
-- >            (defaultPaginationOptions
-- >                |> set #maxItems 10)
-- >    user <- userQ |> fetch
-- >    render IndexView { .. }
paginateWithOptions :: forall controller table queryBuilderProvider joinRegister .
    (?context::ControllerContext
    , ?modelContext :: ModelContext
    , ?theAction :: controller
    , ?request :: Wai.Request
    , KnownSymbol table
    , HasQueryBuilder queryBuilderProvider joinRegister) =>
    Options
    -> queryBuilderProvider table
    -> IO (queryBuilderProvider table, Pagination)
paginateWithOptions options query = do
    count <- query
        |> fetchCount

    let pageSize = pageSize' options
        pagination = Pagination
            { currentPage = page
            , totalItems = fromIntegral count
            , pageSize = pageSize
            , window = windowSize options
            }

    let results = query
            |> limit pageSize
            |> offset (offset' pageSize page)

    pure
        ( results
        , pagination
        )

-- | Reading from the 'filter' query parameter, filters a query according to the string entered in the
--   filter box by the user (if any), on a given text-based field. Will return any results containing the
--   string in a case-insensitive fashion.
--
-- Example:
--
-- > action UsersAction = do
-- >    (userQ, pagination) <- query @User
-- >        |> orderBy #email
-- >        |> paginate
-- >        |> filterList #email
-- >    user <- userQ |> fetch
-- >    render IndexView { .. }
filterList :: forall name table model queryBuilderProvider joinRegister .
    (?context::ControllerContext
    , ?request :: Wai.Request
    , KnownSymbol name
    , HasField name model Text
    , model ~ GetModelByTableName table
    , KnownSymbol table
    , HasQueryBuilder queryBuilderProvider joinRegister
    , Table model
    ) =>
    Proxy name
    -> queryBuilderProvider table
    -> queryBuilderProvider table
filterList field =
    case paramOrNothing @Text "filter" of
       Just uf -> filterWhereILike (field, "%" <> uf <> "%")
       Nothing -> id

-- | Default options for a pagination. Can be passed into 'paginateOptions'. The defaults are as follows:
--
-- 1. Maximum items per page: 50. Each page will show at most 50 items.
-- 2. Selector window size: 5. The selector will show the current page, and up to 5 pages before and after it,
--    if they exist.
defaultPaginationOptions :: Options
defaultPaginationOptions =
    Options
        { maxItems = 50
        , windowSize = 5
        }

-- | Runs a raw sql query and adds pagination to it.
--
-- By default, the pagination uses the following options:
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
-- > (users, pagination) <- paginatedSqlQuery "SELECT id, firstname, lastname FROM users" ()
--
-- Take a look at "IHP.QueryBuilder" for a typesafe approach on building simple queries.
--
-- *AutoRefresh:* When using 'paginatedSqlQuery' with AutoRefresh, you need to use 'trackTableRead' to let AutoRefresh know that you have accessed a certain table. Otherwise AutoRefresh will not watch table of your custom sql query.
paginatedSqlQuery
  :: ( FromRow model
     , ?context :: ControllerContext
     , ?modelContext :: ModelContext
     , ?request :: Wai.Request
     )
  => Snippet.Snippet -> IO ([model], Pagination)
paginatedSqlQuery = paginatedSqlQueryWithOptions defaultPaginationOptions

-- | Runs a raw sql query and adds pagination to it.
--
-- This function accepts the same Options as 'paginateWithOptions', but otherwise behaves like 'paginatedSqlQuery'.
--
-- __Example:__
--
-- > (users, pagination) <- paginatedSqlQueryWithOptions
-- >     (defaultPaginationOptions |> set #maxItems 10)
-- >     "SELECT id, firstname, lastname FROM users"
--
-- Take a look at "IHP.QueryBuilder" for a typesafe approach on building simple queries.
--
-- *AutoRefresh:* When using 'paginatedSqlQuery' with AutoRefresh, you need to use 'trackTableRead' to let AutoRefresh know that you have accessed a certain table. Otherwise AutoRefresh will not watch table of your custom sql query.
paginatedSqlQueryWithOptions
  :: forall model. ( FromRow model
     , ?context :: ControllerContext
     , ?modelContext :: ModelContext
     , ?request :: Wai.Request
     )
  => Options -> Snippet.Snippet -> IO ([model], Pagination)
paginatedSqlQueryWithOptions options baseSnippet = do
    let countSnippet = Snippet.sql "SELECT count(subquery.*) FROM (" <> baseSnippet <> Snippet.sql ") as subquery"
    count :: Int <- sqlQueryScalar countSnippet (Decoders.singleRow (Decoders.column (Decoders.nonNullable (fromIntegral <$> Decoders.int8))))

    let pageSize = pageSize' options
        pagination = Pagination
            { pageSize = pageSize
            , totalItems = fromIntegral count
            , currentPage = fromIntegral page
            , window = windowSize options
            }

    let querySnippet = Snippet.sql "SELECT subquery.* FROM (" <> baseSnippet <> Snippet.sql ") as subquery LIMIT " <> Snippet.param pageSize <> Snippet.sql " OFFSET " <> Snippet.param (offset' pageSize page)
    results :: [model] <- sqlQuery querySnippet (Decoders.rowList (fromRow @model))

    pure (results, pagination)

-- We limit the page size to a maximum of 200, to prevent users from
-- passing in query params with a value that could overload the
-- database (e.g. maxItems=100000)
pageSize' :: (?request :: Wai.Request) => Options -> Int
pageSize' options = min (max 1 $ paramOrDefault @Int (maxItems options) "maxItems") 200

-- Page and page size shouldn't be lower than 1.
page :: (?request :: Wai.Request) => Int
page = max 1 $ paramOrDefault @Int 1 "page"

offset' :: Int -> Int -> Int
offset' pageSize page = (page - 1) * pageSize
