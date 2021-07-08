module IHP.Pagination.ViewFunctions (
    module IHP.Pagination.Types,
    renderPagination,
    renderFilter,
) where

import IHP.Prelude
import IHP.Pagination.Types
import IHP.Pagination.Helpers
import IHP.Pagination.ControllerFunctions

import IHP.ControllerSupport

import Text.Blaze.Html (Html)
import IHP.HSX.QQ (hsx)

import IHP.Controller.Param (paramOrNothing)


-- | Render a navigation for your pagination. This is to be used in your view whenever 
-- to allow users to change pages, including "Next" and "Previous".
renderPagination :: (?context::ControllerContext) => Pagination -> Html
renderPagination pagination@Pagination {currentPage, window, pageSize} =
    [hsx|
        <div class="row justify-content-md-center">
            <div class="col-auto">
            <nav aria-label="Page Navigator">
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
            </div>
            <div class="col-auto">
                <div class="form-row">
                    <div class="col-auto mr-2">
                        <select class="custom-select" id="maxItemsSelect" onchange="changeMaxItems(this);">
                            {maxItemsGenerator}
                        </select>
                    </div>
                </div>
            </div>
        </div>

    |]
        where
            maxItemsGenerator = let
                oneOption :: Int -> Html
                oneOption n = 
                    let
                        selected = n == pageSize
                    in
                    [hsx|<option value={show n} selected={selected}>{n} items per page</option>|]
                in
                    [hsx|{forEach [10,20,50,100,200] oneOption}|]
            
            nextClass = "page-item" ++ (if hasNextPage pagination then "" else " disabled") :: Text
            prevClass = "page-item" ++ (if hasPreviousPage pagination then "" else " disabled") :: Text

            renderItem pg =
                case pg of
                    Page n ->
                        [hsx|<li class={linkClass n}><a class="page-link" href={pageUrl n}>{show n}</a></li>|]
                    DotDot n ->
                        [hsx|<li class="page-item"><a class="page-link" href={pageUrl n}>…</a></li>|]
            linkClass n = "page-item" ++ (if n == currentPage then " active" else "") :: Text
            pageUrl n = "?page=" ++ show n ++ maybeFilter ++ maybeMaxItems
            maybeFilter =
                case paramOrNothing @Text "filter" of
                    Nothing -> ""
                    Just "" -> ""
                    Just u -> "&filter=" ++ u
            maybeMaxItems =
                case paramOrNothing @Text "maxItems" of
                    Nothing -> ""
                    Just "" -> ""
                    Just m -> "&maxItems=" ++ m

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
                        nub $ 1 : [max 1 lowerBound..min (getLastPage pagination) upperBound] ++ [totalPages]

-- | Render a filtering box in your view. Allows the user to type in a query and filter
-- results according to what they type.
--
-- Below is an example of how this might be used in your index. Replace the existing <h1> with:
--        <div class="container">
--          <div class="row justify-content-between">
--              <div class="col-6">
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
                <button onclick="window.location.href = removeURLParameter(window.location.href, 'filter');" class="btn btn-primary mb-2">Clear</button>
                </div>
            </div>
        </form>
    |]
        where
            boxValue = fromMaybe "" (paramOrNothing "filter") :: Text