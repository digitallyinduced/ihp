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
) where

import IHP.Prelude
import IHP.Controller.Context
import IHP.Controller.Param ( paramOrDefault, paramOrNothing )

import IHP.Pagination.Types
    ( Options(..), Pagination(..) )

import IHP.QueryBuilder
    ( QueryBuilder, filterWhereILike, limit, offset )
import IHP.Fetch (fetchCount)

import IHP.ModelSupport (GetModelByTableName)

import Database.PostgreSQL.Simple.ToField


-- Paginate a query, with the following default options:
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
paginate :: (?context::ControllerContext, ?modelContext :: ModelContext, ?theAction :: controller, KnownSymbol q) =>
    QueryBuilder q
    -> IO (QueryBuilder q, Pagination)
paginate = paginateWithOptions defaultPaginationOptions

-- Paginate with ability to override the default options for maximum items per page and selector window size. 
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
paginateWithOptions :: (?context::ControllerContext, ?modelContext :: ModelContext, ?theAction :: controller, KnownSymbol q) =>
    Options
    -> QueryBuilder q
    -> IO (QueryBuilder q, Pagination)
paginateWithOptions options q =
    let page = paramOrDefault @Int 1 "page"
        pageSize = paramOrDefault @Int (maxItems options) "maxItems"
    in do
        count <-
            q |> fetchCount

        let pagination = Pagination
                {
                    currentPage = page
                ,   totalItems = fromIntegral count
                ,   pageSize = pageSize
                ,   window = windowSize options
                }

        let results = q |> limit pageSize |> offset ((page - 1) * pageSize)

        pure
            ( results
            , pagination
            )

-- | Reading from the 'filter' query parameter, filters a query according to the string entered in the 
--   filter box by the user (if any), on a given text-based field. Will return any results containing the
--   string in a case-insenstive fashion. 
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
filterList :: forall name table model . (?context::ControllerContext, KnownSymbol name, HasField name model Text, model ~ GetModelByTableName table) =>
    Proxy name
    -> QueryBuilder table
    -> QueryBuilder table
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
        {
            maxItems = 50
        ,   windowSize = 5
        }