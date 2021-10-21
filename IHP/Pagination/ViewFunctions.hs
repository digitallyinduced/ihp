module IHP.Pagination.ViewFunctions (
    module IHP.Pagination.Types,
    renderPagination,
    renderFilter,
) where

import IHP.Prelude
import IHP.Pagination.Types
import IHP.Pagination.Helpers

import IHP.ControllerSupport

import Text.Blaze.Html (Html)
import IHP.HSX.QQ (hsx)

import IHP.Controller.Param (paramOrNothing)

import IHP.View.Classes
import qualified Network.Wai as Wai
import qualified Network.HTTP.Types.URI as Query
import IHP.ViewSupport (theRequest, theCSSFramework)
import qualified Data.Containers.ListUtils as List
import IHP.View.Types (PaginationView(..), styledPagination, styledPaginationPageLink, styledPaginationDotDot)
import IHP.View.CSSFramework


-- | Render a navigation for your pagination. This is to be used in your view whenever
-- to allow users to change pages, including "Next" and "Previous".
renderPagination :: (?context::ControllerContext) => Pagination -> Html
renderPagination pagination@Pagination {currentPage, window, pageSize} =
    [hsx|
        <div class="d-flex justify-content-md-center">
            <nav aria-label="Page Navigator" class="mr-2">
                <ul class="pagination">
                    <li class={prevClass}>
                        <a class="page-link" href={pageUrl $ currentPage - 1} aria-label="Previous">
                            <span aria-hidden="true">&laquo;</span>
                            <span class="sr-only">Previous</span>
                        </a>
                    </li>
                    {renderItems}
                    <li class={nextClass}>
                        <a class="page-link" href={pageUrl $ currentPage + 1} aria-label="Previous">
                            <span aria-hidden="true">&raquo;</span>
                            <span class="sr-only">Next</span>
                        </a>
                    </li>
                </ul>
            </nav>
            <div class="form-row">
                <div class="col-auto mr-2">
                    <select class="custom-select" id="maxItemsSelect" onchange="window.location.href = this.options[this.selectedIndex].dataset.url">
                        {maxItemsGenerator}
                    </select>
                </div>
            </div>
        </div>
    |]
        where
            paginationView = PaginationView
                { cssFramework = theCSSFramework
                , pagination = pagination
                , previousPageUrl = pageUrl $ currentPage - 1
                , nextPageUrl = pageUrl $ currentPage + 1
                }

            renderedHtml = styledPagination theCSSFramework theCSSFramework paginationView

            maxItemsGenerator = let
                oneOption :: Int -> Html
                oneOption n = [hsx|<option value={show n} selected={n == pageSize} data-url={itemsPerPageUrl n}>{n} items per page</option>|]
                in
                    [hsx|{forEach [10,20,50,100,200] oneOption}|]

            nextClass = classes ["page-item", ("disabled", not $ hasNextPage pagination)]
            prevClass = classes ["page-item", ("disabled", not $ hasPreviousPage pagination)]

            -- renderItem pg =
            --    case pg of
            --        Page n ->
            --            [hsx|<li class={linkClass n}><a class="page-link" href={pageUrl n}>{show n}</a></li>|]
            --        DotDot n ->
            --             [hsx|<li class="page-item"><a class="page-link" href={pageUrl n}>…</a></li>|]

            -- @todo:
            -- linkClass n = classes ["page-item", ("active", n == currentPage)]

            renderItem pg =
                case pg of
                    Page n ->
                        styledPaginationPageLink theCSSFramework theCSSFramework paginationView (pageUrl n) n
                    DotDot n ->
                        styledPaginationDotDot theCSSFramework theCSSFramework paginationView (pageUrl n) n



            pageUrl n = path <> Query.renderQuery True newQueryString
                where
                    -- "?page=" ++ show n ++ maybeFilter ++ maybeMaxItems
                    path = Wai.rawPathInfo theRequest
                    queryString = Wai.queryString theRequest
                    newQueryString = queryString
                        |> setQueryValue "page" (cs $ show n)
                        |> maybeFilter
                        |> maybeMaxItems

            itemsPerPageUrl n = path <> Query.renderQuery True newQueryString
                where
                    path = Wai.rawPathInfo theRequest
                    queryString = Wai.queryString theRequest
                    newQueryString = queryString
                        |> setQueryValue "maxItems" (cs $ tshow n)

            maybeFilter queryString =
                case paramOrNothing @Text "filter" of
                    Nothing -> queryString
                    Just "" -> queryString
                    Just filterValue -> queryString |> setQueryValue "filter" (cs filterValue)

            maybeMaxItems queryString =
                case paramOrNothing @Int "maxItems" of
                    Nothing -> queryString
                    Just m -> queryString |> setQueryValue "maxItems" (cs $ tshow m)

            renderItems = [hsx|{forEach (processedPages pages) renderItem}|]

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
-- results according to what they type.
--
-- Below is an example of how this might be used in your index. Replace the existing <h1> with:
--        <div class="container">
--          <div class="row justify-content-between">
--              <div class="col-7">
--                  <h1>Users<a href={pathTo NewUserAction} class="btn btn-primary ml-4">+ New</a></h1>
--              </div>
--              <div class="col-5">
--                  {renderFilter "Username"}
--              </div>
--          </div>
--        </div>
renderFilter :: (?context::ControllerContext) =>
    Text    -- ^ Placeholder text for the text box
    -> Html
renderFilter placeholder =
    [hsx|
        <form method="GET" action="" class="mt-2 float-right">
            <div class="form-row">
                <div class="col-auto">
                <label class="sr-only" for="inlineFormInput">Name</label>
                <input type="hidden" name="page" value="1"/>
                <input name="filter" type="text" class="form-control mb-2" id="inlineFormInput" placeholder={placeholder} value={boxValue}>
                </div>
                <div class="col-auto">
                    <button type="submit" class="btn btn-primary mb-2 mr-2">Filter</button>
                    <a class="btn btn-primary mb-2" href={clearFilterUrl}>Clear</a>
                </div>
            </div>
        </form>
    |]
        where
            boxValue = fromMaybe "" (paramOrNothing "filter") :: Text
            clearFilterUrl = path <> Query.renderQuery True newQueryString
                where
                    path = Wai.rawPathInfo theRequest
                    queryString = Wai.queryString theRequest
                    newQueryString = queryString
                        |> removeQueryItem "filter"


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
