module IHP.Breadcrumb.ViewFunctions (
    module IHP.Breadcrumb.Types,
    renderBreadcrumbs,
) where

import IHP.Prelude
import IHP.Breadcrumb.Types

import IHP.ControllerSupport

import Text.Blaze.Html (Html)
import IHP.HSX.QQ (hsx)

import IHP.View.Types (BreadcrumbsView(..), styledBreadcrumbs, styledBreadcrumbItem)
import IHP.ViewSupport (theCSSFramework)
import IHP.ControllerPrelude

-- | Render breadcrumbs.
renderBreadcrumbs :: (?context::ControllerContext) => [BreadcrumbItem] -> Html
renderBreadcrumbs breadcrumbItems = styledBreadcrumbs theCSSFramework theCSSFramework breadcrumbItems breadcrumbsView
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