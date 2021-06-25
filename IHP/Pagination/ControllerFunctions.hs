{-|
Module: IHP.PageTitle.ControllerFunctions
Description: Paginate results in your actions
Copyright: (c) digitally induced GmbH, 2021
-}
module IHP.Pagination.ControllerFunctions
(   paginate, 
    paginateOptions, 
    filterList,
    defaultPaginationOptions,
    itemsPerPage,
    windowSize,
    setItemsPerPage,
    setWindowSize
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


-- Paginate a query. 
--
-- This function should be used inside your controller action. It will do two things: 
--  
--     1. Applies the correct limit and offset to display the correct page according to the
--     current parameters.
--     2. Returns a 'Pagination' state which should be passed through to your view and then,
--         in turn 'renderPagination'
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
paginate = paginateOptions defaultPaginationOptions

-- Paginate with custom options. 
--
-- This function should be used inside your controller action. It will do two things: 
--  
--     1. Applies the correct limit and offset to display the correct page according to the
--     current parameters.
--     2. Returns a 'Pagination' state which should be passed through to your view and then,
--         in turn 'renderPagination'
--
-- Example:
--
-- > action UsersAction = do
-- >    (userQ, pagination) <- query @User 
-- >        |> orderBy #email
-- >        |> paginateOptions (itemsPerPage 5)
-- >    user <- userQ |> fetch
-- >    render IndexView { .. }
paginateOptions :: (?context::ControllerContext, ?modelContext :: ModelContext, ?theAction :: controller, KnownSymbol q) =>
    Options
    -> QueryBuilder q
    -> IO (QueryBuilder q, Pagination)
paginateOptions options q =
    let page = paramOrDefault @Int 1 "page"
        pageSize = paramOrDefault @Int (maxItemsOption options) "maxItems"
        windowSize = windowSizeOption options
    in do
        count <-
            q |> fetchCount

        let pagination = Pagination
                {
                    currentPage = page
                ,   totalItems = fromIntegral count
                ,   pageSize = pageSize
                ,   window = windowSize
                }

        let results = q |> limit pageSize |> offset ((page - 1) * pageSize)

        pure
            ( results
            , pagination
            )

-- | Filter a query according to the query entered in the filter box by the user (if any),
-- on a given text-based field.
--
-- Example:
-- 
-- > action UsersAction = do
-- >    (userQ, pagination) <- query @User 
-- >        |> orderBy #email
-- >        |> paginateOptions (itemsPerPage 5)
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

-- | Default options for a pagination. Can be passed into 'paginateOptions'.
defaultPaginationOptions :: Options
defaultPaginationOptions =
    Options 
        {
            maxItemsOption = 100
        ,   windowSizeOption = 5
        }

-- | Customize the default number of items per page. Can be 
--  passed into 'paginateOptions'.
itemsPerPage :: Int -> Options
itemsPerPage n =
    Options
        {
            maxItemsOption = n
        ,   windowSizeOption = 5
        }

-- | Customize the ``window size'', which is the radius of the 
--   pages shown on the navbar. Can be passed into 'paginateOptions'.
windowSize :: Int -> Options
windowSize n =
    Options
        {
            maxItemsOption = 100
        ,   windowSizeOption = n
        }

-- | Customize the default number of items per page in an existing 
--  'Options'. Can be used as part of 'paginateOptions':
--
-- > (userQ, pagination) <- query @User 
-- >        |> orderBy #email
-- >        |> paginateOptions 
-- >            (defaultOptions |> setItemsPerPage 5)
setItemsPerPage :: Int -> Options -> Options
setItemsPerPage n options =
    options
        {
            maxItemsOption = n
        }

-- | Customize the ``window size'' in an exsiting 'Options', which 
--   is the radius of the pages shown on the navbar. Can be used as 
--   part of 'paginateOptions':
--
-- > (userQ, pagination) <- query @User 
-- >        |> orderBy #email
-- >        |> paginateOptions 
-- >            (defaultOptions |> setWindowSize 3)
setWindowSize :: Int -> Options -> Options
setWindowSize n options =
    options
        {
            windowSizeOption = n
        }