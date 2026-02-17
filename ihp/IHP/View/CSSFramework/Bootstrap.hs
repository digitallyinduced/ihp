{-|
Module: IHP.View.CSSFramework.Bootstrap
Description: Bootstrap CSS framework implementations
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.View.CSSFramework.Bootstrap (bootstrap, bootstrap4) where

import Prelude hiding (null)
import Data.Text (Text, null)
import Data.ByteString (ByteString)
import IHP.HaskellSupport (forEach)
import qualified Text.Blaze.Html5 as Blaze
import IHP.HSX.QQ (hsx)
import IHP.HSX.ToHtml ()
import IHP.View.Types
import IHP.View.Classes
import IHP.Breadcrumb.Types
import IHP.Pagination.Helpers
import IHP.Pagination.Types
import Network.Wai.Middleware.FlashMessages (FlashMessage (..))
import IHP.View.CSSFramework.Unstyled (unstyled)

bootstrap :: CSSFramework
bootstrap = unstyled
    { styledFlashMessage
    , styledSubmitButtonClass
    , styledFormGroupClass
    , styledFormFieldHelp
    , styledInputClass
    , styledInputInvalidClass
    , styledValidationResultClass
    }
    where
        styledFlashMessage _ (SuccessFlashMessage message) = [hsx|<div class="alert alert-success">{message}</div>|]
        styledFlashMessage _ (ErrorFlashMessage message) = [hsx|<div class="alert alert-danger">{message}</div>|]

        styledInputClass _ FormField { fieldType = FileInput } = "form-control-file"
        styledInputClass _ FormField {} = "form-control"
        styledInputInvalidClass _ _ = "is-invalid"

        styledFormFieldHelp _ FormField { helpText = "" } = mempty
        styledFormFieldHelp _ FormField { helpText } = [hsx|<small class="form-text">{helpText}</small>|]

        styledFormGroupClass = "mb-3"

        styledValidationResultClass = "invalid-feedback"

        styledSubmitButtonClass = "btn btn-primary"

bootstrap4 :: CSSFramework
bootstrap4 = unstyled
    { styledFlashMessage
    , styledLabelClass
    , styledSubmitButtonClass
    , styledFormFieldHelp
    , styledInputClass
    , styledInputInvalidClass
    , styledFormGroupClass
    , styledValidationResultClass
    , styledPagination
    , styledPaginationPageLink
    , styledPaginationDotDot
    , styledPaginationItemsPerPageSelector
    , styledPaginationLinkPrevious
    , styledPaginationLinkNext
    , styledBreadcrumb
    , styledBreadcrumbItem
    }
    where
        styledFlashMessage _ (SuccessFlashMessage message) = [hsx|<div class="alert alert-success">{message}</div>|]
        styledFlashMessage _ (ErrorFlashMessage message) = [hsx|<div class="alert alert-danger">{message}</div>|]

        styledInputClass _ FormField { fieldType = FileInput } = "form-control-file"
        styledInputClass _ FormField {} = "form-control"
        styledInputInvalidClass _ _ = "is-invalid"

        styledFormFieldHelp _ FormField { helpText = "" } = mempty
        styledFormFieldHelp _ FormField { helpText } = [hsx|<small class="form-text text-muted">{helpText}</small>|]

        styledFormGroupClass = "form-group"
        styledValidationResultClass = "invalid-feedback"
        styledSubmitButtonClass = "btn btn-primary"
        styledLabelClass = ""

        styledPagination :: CSSFramework -> PaginationView -> Blaze.Html
        styledPagination _ paginationView =
            [hsx|

            <div class="d-flex justify-content-md-center">
                <nav aria-label="Page Navigator" class="mr-2">
                    <ul class="pagination">
                        {paginationView.linkPrevious}
                        {paginationView.pageDotDotItems}
                        {paginationView.linkNext}
                    </ul>
                </nav>

                <div class="form-row">
                    <div class="col-auto mr-2">
                        <select class="custom-select" id="maxItemsSelect" onchange="window.location.href = this.options[this.selectedIndex].dataset.url">
                            {paginationView.itemsPerPageSelector}
                        </select>
                    </div>
                </div>

            </div>
            |]

        styledPaginationPageLink :: CSSFramework -> Pagination -> ByteString -> Int -> Blaze.Html
        styledPaginationPageLink _ pagination@Pagination {currentPage} pageUrl pageNumber =
            let
                linkClass = classes ["page-item", ("active", pageNumber == currentPage)]
            in
                [hsx|<li class={linkClass}><a class="page-link" href={pageUrl}>{show pageNumber}</a></li>|]


        styledPaginationDotDot :: CSSFramework -> Pagination -> Blaze.Html
        styledPaginationDotDot _ _ =
            [hsx|<li class="page-item"><a class="page-link">…</a></li>|]

        styledPaginationItemsPerPageSelector :: CSSFramework -> Pagination -> (Int -> ByteString) -> Blaze.Html
        styledPaginationItemsPerPageSelector _ pagination@Pagination {pageSize} itemsPerPageUrl =
            let
                oneOption :: Int -> Blaze.Html
                oneOption n = [hsx|<option value={show n} selected={n == pageSize} data-url={itemsPerPageUrl n}>{n} items per page</option>|]
            in
                [hsx|{forEach [10,20,50,100,200] oneOption}|]

        styledPaginationLinkPrevious :: CSSFramework -> Pagination -> ByteString -> Blaze.Html
        styledPaginationLinkPrevious _ pagination@Pagination {currentPage} pageUrl =
            let
                prevClass = classes ["page-item", ("disabled", not $ hasPreviousPage pagination)]
                url = if hasPreviousPage pagination then pageUrl else "#"
            in
                [hsx|
                    <li class={prevClass}>
                        <a class="page-link" href={url} aria-label="Previous">
                            <span aria-hidden="true">&laquo;</span>
                            <span class="sr-only">Previous</span>
                        </a>
                    </li>
                |]

        styledPaginationLinkNext :: CSSFramework -> Pagination -> ByteString -> Blaze.Html
        styledPaginationLinkNext _ pagination@Pagination {currentPage} pageUrl =
            let
                nextClass = classes ["page-item", ("disabled", not $ hasNextPage pagination)]
                url = if hasNextPage pagination then pageUrl else "#"
            in
                [hsx|
                    <li class={nextClass}>
                        <a class="page-link" href={url} aria-label="Next">
                            <span aria-hidden="true">&raquo;</span>
                            <span class="sr-only">Next</span>
                        </a>
                    </li>
                |]

        styledBreadcrumb :: CSSFramework -> [BreadcrumbItem]-> BreadcrumbsView -> Blaze.Html
        styledBreadcrumb _ _ breadcrumbsView = [hsx|
            <nav>
                <ol class="breadcrumb">
                    {breadcrumbsView.breadcrumbItems}

                </ol>
            </nav>
        |]


        styledBreadcrumbItem :: CSSFramework -> [ BreadcrumbItem ]-> BreadcrumbItem -> Bool -> Blaze.Html
        styledBreadcrumbItem _ breadcrumbItems breadcrumbItem@BreadcrumbItem {breadcrumbLabel, url} isLast =
            let
                breadcrumbsClasses = classes ["breadcrumb-item", ("active", isLast)]
            in
            case url of
                Nothing ->  [hsx|<li class={breadcrumbsClasses}>{breadcrumbLabel}</li>|]
                Just url -> [hsx|<li class={breadcrumbsClasses}><a href={url}>{breadcrumbLabel}</a></li>|]
