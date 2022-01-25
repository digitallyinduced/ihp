module IHP.Breadcrumb.ViewFunctions (
    module IHP.Breadcrumb.Types,
    renderBreadcrumb,
    breadcrumbLink,
    breadcrumbLinkExternal,
    breadcrumbText,
) where

import IHP.Prelude
import IHP.Breadcrumb.Types

import IHP.ControllerSupport

import Text.Blaze.Html (Html)

import IHP.View.Types (BreadcrumbsView(..), styledBreadcrumb, styledBreadcrumbItem)
import IHP.ViewSupport (theCSSFramework)
import IHP.ControllerPrelude

renderBreadcrumb :: (?context :: ControllerContext) => [BreadcrumbItem] -> Html
renderBreadcrumb breadcrumbItems = styledBreadcrumb theCSSFramework theCSSFramework breadcrumbItems breadcrumbsView
        where
            breadcrumbsView = BreadcrumbsView
                { breadcrumbItems = forEachWithIndex breadcrumbItems (\(index, breadcrumbItem) ->
                    let
                        isLast = index == length breadcrumbItems - 1
                    in
                    styledBreadcrumbItem theCSSFramework theCSSFramework breadcrumbItems breadcrumbItem isLast
                    )
                }


breadcrumbLink :: (HasPath controller) =>Html -> controller -> BreadcrumbItem
breadcrumbLink label route =
    breadcrumbLinkExternal label (pathTo route)

breadcrumbLinkExternal :: Html -> Text -> BreadcrumbItem
breadcrumbLinkExternal label url =
    BreadcrumbItem { breadcrumbLabel = label, url = Just url }



breadcrumbText :: Html -> BreadcrumbItem
breadcrumbText label =
        BreadcrumbItem { breadcrumbLabel = label, url = Nothing }