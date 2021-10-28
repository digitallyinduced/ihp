module IHP.Breadcrumb.ViewFunctions (
    module IHP.Breadcrumb.Types,
    renderBreadcrumb,
    breadcrumbWithLink,
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
    BreadcrumbItem { label = label, url = Just $ pathTo route }


breadcrumbWithoutLink :: Html -> BreadcrumbItem
breadcrumbWithoutLink label =
        BreadcrumbItem { label = label, url = Nothing }