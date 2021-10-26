module IHP.Breadcrumbs.ViewFunctions (
    module IHP.Breadcrumbs.Types,
    renderBreadcrumbs,
) where

import IHP.Prelude
import IHP.Breadcrumbs.Types

import IHP.ControllerSupport

import Text.Blaze.Html (Html)
import IHP.HSX.QQ (hsx)

import IHP.View.Types (BreadcrumbsView(..), styledBreadcrumbs, styledBreadcrumbsItem)
import IHP.ViewSupport (theCSSFramework)

-- | Render breadcrumbs.
renderBreadcrumbs :: (?context::ControllerContext) => [BreadcrumbsItem] -> Html
renderBreadcrumbs breadcrumbsItems = [hsx| {renderedHtml} |]
        where
            breadcrumbsView = BreadcrumbsView
                { cssFramework = theCSSFramework
                , breadcrumbsItems = breadcrumbsItemsRendered
                }

            renderedHtml = styledBreadcrumbs theCSSFramework theCSSFramework breadcrumbsItems breadcrumbsView

            breadcrumbsItemsRendered =  [hsx|{forEach breadcrumbsItems (styledBreadcrumbsItem theCSSFramework theCSSFramework breadcrumbsItems)}|]


