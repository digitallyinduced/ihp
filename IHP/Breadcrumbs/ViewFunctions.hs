module IHP.Breadcrumbs.ViewFunctions (
    module IHP.Breadcrumbs.Types,
    renderBreadcrumbs,
) where

import IHP.Prelude
import IHP.Breadcrumbs.Types

import IHP.ControllerSupport

import Text.Blaze.Html (Html)
import IHP.HSX.QQ (hsx)

import IHP.View.Types (BreadcrumbsView(..), styledBreadcrumbs, styledBreadcrumbItem)
import IHP.ViewSupport (theCSSFramework)

-- | Render breadcrumbs.
renderBreadcrumbs :: (?context::ControllerContext) => [BreadcrumbItem] -> Html
renderBreadcrumbs breadcrumbItems = [hsx| {renderedHtml} |]
        where
            breadcrumbsView = BreadcrumbsView
                { cssFramework = theCSSFramework
                , breadcrumbItems = breadcrumbItemsRendered
                }

            renderedHtml = styledBreadcrumbs theCSSFramework theCSSFramework breadcrumbItems breadcrumbsView

            breadcrumbItemsRendered =  [hsx|{forEach breadcrumbItems (styledBreadcrumbItem theCSSFramework theCSSFramework breadcrumbItems)}|]


