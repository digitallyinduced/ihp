module IHP.Breadcrumb.ViewFunctions (
    module IHP.Breadcrumb.Types,
    renderBreadcrumb,
    breadcrumbWithLink,
    breadcrumbWithExternalLink,
    breadcrumbWithoutLink,
) where

import IHP.Prelude
import IHP.Breadcrumb.Types

import IHP.ControllerSupport

import Text.Blaze.Html (Html)

import IHP.View.Types (BreadcrumbsView(..), styledBreadcrumbs, styledBreadcrumbItem)
import IHP.ViewSupport (theCSSFramework)
import IHP.ControllerPrelude

renderBreadcrumb :: (?context::ControllerContext) => [BreadcrumbItem] -> Html
renderBreadcrumb breadcrumbItems = styledBreadcrumbs theCSSFramework theCSSFramework breadcrumbItems breadcrumbsView
        where
            breadcrumbsView = BreadcrumbsView
                { cssFramework = theCSSFramework
                , breadcrumbItems = breadcrumbItemsRendered
                }

            breadcrumbItemsRendered =  [hsx|{forEach breadcrumbItems (styledBreadcrumbItem theCSSFramework theCSSFramework breadcrumbItems)}|]


breadcrumbWithLink :: (Show controller, AutoRoute controller) => Html -> controller -> BreadcrumbItem
breadcrumbWithLink label route =
    breadcrumbWithExternalLink label (pathTo route)

breadcrumbWithExternalLink :: Html -> Text -> BreadcrumbItem
breadcrumbWithExternalLink label url =
    BreadcrumbItem { label = label, url = Just url }



breadcrumbWithoutLink :: Html -> BreadcrumbItem
breadcrumbWithoutLink label =
        BreadcrumbItem { label = label, url = Nothing }