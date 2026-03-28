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

import IHP.HSX.Markup (Markup)
import IHP.View.Types (BreadcrumbsView(..), styledBreadcrumb, styledBreadcrumbItem)
import IHP.ViewSupport (theCSSFramework)
import IHP.ControllerPrelude

renderBreadcrumb :: (?request :: Request) => [BreadcrumbItem] -> Markup
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


breadcrumbLink :: (HasPath controller) => Markup -> controller -> BreadcrumbItem
breadcrumbLink label route =
    breadcrumbLinkExternal label (pathTo route)

breadcrumbLinkExternal :: Markup -> Text -> BreadcrumbItem
breadcrumbLinkExternal label url =
    BreadcrumbItem { breadcrumbLabel = label, url = Just url }



breadcrumbText :: Markup -> BreadcrumbItem
breadcrumbText label =
        BreadcrumbItem { breadcrumbLabel = label, url = Nothing }