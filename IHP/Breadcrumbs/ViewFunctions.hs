module IHP.Breadcrumbs.ViewFunctions (
    module IHP.Breadcrumbs.Types,
    renderBreadcrumbs,
) where

import IHP.Prelude
import IHP.Breadcrumbs.Types

import IHP.ControllerSupport

import Text.Blaze.Html (Html)
import IHP.HSX.QQ (hsx)


import IHP.View.Classes
import IHP.View.Types (BreadcrumbsView(..), styleBreadcrumbs, styleBreadcrumbsItem)
import IHP.View.CSSFramework
import IHP.ViewSupport (theCSSFramework)

-- | Render breadcrumbs.
renderBreadcrumbs :: (?context::ControllerContext) => [BreadcrumbsItem] -> Html
renderBreadcrumbs breadcrumbsItems = [hsx| {renderedHtml} |]
        where
            breadcrumbsView = BreadcrumbsView
                { cssFramework = theCSSFramework
                , breadcrumbsItems = breadcrumbsItemsRendered
                }

            renderedHtml = styleBreadcrumbs theCSSFramework theCSSFramework breadcrumbsItems breadcrumbsView

            breadcrumbsItemsRendered =  [hsx|{forEach breadcrumbsItems (styleBreadcrumbsItem theCSSFramework theCSSFramework breadcrumbsItems)}|]


