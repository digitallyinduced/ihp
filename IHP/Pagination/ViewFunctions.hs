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
import IHP.View.Types (PaginationView(..), styledPagination, styledPaginationPageLink, styledPaginationDotDot, stylePaginationItemsPerPageSelector, styledPaginationLiPrevious, styledPaginationLiNext)
import IHP.View.CSSFramework


-- | Render a navigation for your pagination. This is to be used in your view whenever
-- to allow users to change pages, including "Next" and "Previous".
renderPagination :: (?context::ControllerContext) => Pagination -> Html
renderPagination pagination@Pagination {currentPage, window, pageSize} = [hsx| {renderedHtml} |]
        where
            paginationView = PaginationView
                { cssFramework = theCSSFramework
                , pagination = pagination
                , linkPrevious = linkPrevious
                , linkNext = linkNext
                , pageDotDotItems = pageDotDotItems
                , itemsPerPageSelector = itemsPerPageSelector
                }

            renderedHtml = styledPagination theCSSFramework theCSSFramework paginationView

            linkPrevious =
                styledPaginationLiPrevious theCSSFramework theCSSFramework pagination (pageUrl $ currentPage - 1)

            linkNext =
                styledPaginationLiNext theCSSFramework theCSSFramework pagination (pageUrl $ currentPage + 1)

            itemsPerPageSelector =
                stylePaginationItemsPerPageSelector theCSSFramework theCSSFramework pagination itemsPerPageUrl

            pageDotDotItems = [hsx|{forEach (processedPages pages) pageDotDotItem}|]

            pageDotDotItem pg =
                case pg of
                    Page n ->
                        styledPaginationPageLink theCSSFramework theCSSFramework pagination (pageUrl n) n
                    DotDot n ->
                        styledPaginationDotDot theCSSFramework theCSSFramework pagination (pageUrl n) n

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
                        -- If we change the number of items, we should jump back to the first page
                        -- so we are not out of the items bound.
                        |> setQueryValue "page" (cs $ show 1)

            maybeFilter queryString =
                case paramOrNothing @Text "filter" of
                    Nothing -> queryString
                    Just "" -> queryString
                    Just filterValue -> queryString |> setQueryValue "filter" (cs filterValue)

            maybeMaxItems queryString =
                case paramOrNothing @Int "maxItems" of
                    Nothing -> queryString
                    Just m -> queryString |> setQueryValue "maxItems" (cs $ tshow m)

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
