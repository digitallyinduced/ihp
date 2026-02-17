module IHP.Pagination.ViewFunctions (
    module IHP.Pagination.Types,
    renderPagination,
    renderFilter,
    renderFilterFor,
) where

import IHP.Prelude
import IHP.Pagination.Types
import IHP.Pagination.Helpers

import IHP.ControllerSupport

import Text.Blaze.Html (Html)
import IHP.HSX.QQ (hsx)

import IHP.Controller.Param (paramOrNothing)

import Network.Wai
import qualified Network.HTTP.Types.URI as Query
import IHP.ViewSupport (theRequest, theCSSFramework)
import qualified Data.Containers.ListUtils as List
import IHP.View.Types (PaginationView(..), styledPagination, styledPaginationPageLink, styledPaginationDotDot, styledPaginationItemsPerPageSelector, styledPaginationLinkPrevious, styledPaginationLinkNext)


-- | Render a navigation for your pagination. This is to be used in your view whenever
-- to allow users to change pages, including "Next" and "Previous".
-- If there is only one page, this will not render anything.
renderPagination :: (?context :: ControllerContext, ?request :: Request) => Pagination -> Html
renderPagination pagination@Pagination {currentPage, window, pageSize, paramSuffix} =
        when (showPagination pagination) $ styledPagination theCSSFramework theCSSFramework paginationView
        where
            suffix = paramSuffix

            paginationView = PaginationView
                { cssFramework = theCSSFramework
                , pagination = pagination
                , pageUrl = pageUrl
                , linkPrevious = linkPrevious
                , linkNext = linkNext
                , pageDotDotItems = pageDotDotItems
                , itemsPerPageSelector = itemsPerPageSelector
                , paramSuffix = suffix
                }

            linkPrevious =
                styledPaginationLinkPrevious theCSSFramework theCSSFramework pagination (pageUrl $ currentPage - 1)

            linkNext =
                styledPaginationLinkNext theCSSFramework theCSSFramework pagination (pageUrl $ currentPage + 1)

            itemsPerPageSelector =
                styledPaginationItemsPerPageSelector theCSSFramework theCSSFramework pagination itemsPerPageUrl

            pageDotDotItems = [hsx|{forEach (processedPages pages) pageDotDotItem}|]

            pageDotDotItem pg =
                case pg of
                    Page n ->
                        styledPaginationPageLink theCSSFramework theCSSFramework pagination (pageUrl n) n
                    DotDot n ->
                        styledPaginationDotDot theCSSFramework theCSSFramework pagination

            pageUrl n = path <> Query.renderQuery True newQueryString
                where
                    -- "?page=" ++ show n ++ maybeFilter ++ maybeMaxItems
                    path = theRequest.rawPathInfo
                    queryString = theRequest.queryString
                    newQueryString = queryString
                        |> setQueryValue (cs $ "page" <> suffix) (cs $ show n)
                        |> maybeFilter
                        |> maybeMaxItems

            itemsPerPageUrl n = path <> Query.renderQuery True newQueryString
                where
                    path = theRequest.rawPathInfo
                    queryString = theRequest.queryString
                    newQueryString = queryString
                        |> setQueryValue (cs $ "maxItems" <> suffix) (cs $ tshow n)
                        -- If we change the number of items, we should jump back to the first page
                        -- so we are not out of the items bound.
                        |> setQueryValue (cs $ "page" <> suffix) (cs $ show 1)

            maybeFilter queryString =
                case paramOrNothing @Text ("filter" <> suffix) of
                    Nothing -> queryString
                    Just "" -> queryString
                    Just filterValue -> queryString |> setQueryValue (cs $ "filter" <> suffix) (cs filterValue)

            maybeMaxItems queryString =
                case paramOrNothing @Int ("maxItems" <> suffix) of
                    Nothing -> queryString
                    Just m -> queryString |> setQueryValue (cs $ "maxItems" <> suffix) (cs $ tshow m)

            processedPages (pg0:pg1:rest) =
                if pg1 == pg0 + 1 then
                    Page pg0 : processedPages (pg1:rest)
                else
                    Page pg0 : DotDot ((pg1+pg0) `div` 2) : processedPages (pg1:rest)
            processedPages [pg] =
                [Page pg]
            processedPages [] = []
            pages =
                let
                    totalPages = getLastPage pagination

                    lowerBound
                      | currentPage - window < 1 =
                        1
                      | currentPage + window > totalPages =
                        totalPages - window * 2 + 1
                      | otherwise =
                        currentPage - window

                    upperBound
                      | currentPage + window > totalPages =
                        totalPages
                      | currentPage - window < 1 =
                        window * 2
                      | otherwise =
                        currentPage + window

                in
                    if window > totalPages then
                        [1..getLastPage pagination]
                    else
                        List.nubInt $ 1 : [max 1 lowerBound..min (getLastPage pagination) upperBound] ++ [totalPages]

-- | Render a filtering box in your view. Allows the user to type in a query and filter
-- results according to what they type. Uses no suffix (backward compatible).
--
-- For multiple paginations per page, use 'renderFilterFor' instead.
--
-- Below is an example of how this might be used in your index. Replace the existing <h1> with:
--        <div class="container">
--          <div class="row justify-content-between">
--              <div class="col-7">
--                  <h1>Users<a href={pathTo NewUserAction} class="btn btn-primary ms-4">+ New</a></h1>
--              </div>
--              <div class="col-5">
--                  {renderFilter "Username"}
--              </div>
--          </div>
--        </div>
renderFilter :: (?context::ControllerContext, ?request :: Request) =>
    Text    -- ^ Placeholder text for the text box
    -> Html
renderFilter = renderFilterFor ""

-- | Like 'renderFilter', but takes a 'Pagination' to use the correct query parameter suffix.
-- Use this when you have multiple paginations on the same page.
--
-- Example:
--
-- > {renderFilterFor pagination.paramSuffix "Username"}
renderFilterFor :: (?context::ControllerContext, ?request :: Request) =>
    Text    -- ^ The param suffix from a 'Pagination' (e.g. @""@, @"_2"@)
    -> Text -- ^ Placeholder text for the text box
    -> Html
renderFilterFor suffix placeholder =
    [hsx|
        <form method="GET" action="" class="mt-2 float-end">
            <div class="row">
                <div class="col-auto">
                <label class="visually-hidden" for={inputId}>Name</label>
                <input type="hidden" name={pageParam} value="1"/>
                <input name={filterParam} type="text" class="form-control mb-2" id={inputId} placeholder={placeholder} value={boxValue}>
                </div>
                <div class="col-auto">
                    <button type="submit" class="btn btn-primary mb-2 me-2">Filter</button>
                    <a class="btn btn-primary mb-2" href={clearFilterUrl}>Clear</a>
                </div>
            </div>
        </form>
    |]
        where
            pageParam = "page" <> suffix :: Text
            filterParam = "filter" <> suffix :: Text
            inputId = "inlineFormInput" <> suffix :: Text
            boxValue = fromMaybe "" (paramOrNothing filterParam) :: Text
            clearFilterUrl = path <> Query.renderQuery True newQueryString
                where
                    path = theRequest.rawPathInfo
                    queryString = theRequest.queryString
                    newQueryString = queryString
                        |> removeQueryItem (cs filterParam)


-- | Set or replace a query string item
--
-- >>> setQueryValue "page" "1" []
-- [("page", Just "1")]
--
-- >>> setQueryValue "page" "2" [("page", Just "1")]
-- [("page", Just "2")]
--
setQueryValue :: ByteString -> ByteString -> Query.Query -> Query.Query
setQueryValue name value queryString =
    case lookup name queryString of
        Just existingPage -> queryString
                |> map (\(queryItem@(queryItemName, _)) -> if queryItemName == name
                        then (name, Just value)
                        else queryItem
                    )
        Nothing -> queryString <> [(name, Just value)]

-- | Removes a query item, specified by the name
--
-- >>> removeQueryItem "filter" [("filter", Just "test")]
-- []
--
removeQueryItem :: ByteString -> Query.Query -> Query.Query
removeQueryItem name queryString = queryString |> filter (\(queryItemName, _) -> queryItemName /= name)

{-| Determine if a Pagination needs to be shown.
    If there is only a single page, we shouldn't show a pager.
-}
showPagination :: Pagination -> Bool
showPagination pagination@Pagination {currentPage} =
    currentPage /= 1 || hasNextPage pagination || hasPreviousPage pagination
