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
import IHP.View.Types (styleBreadcrumbsItem)
import IHP.View.CSSFramework
import IHP.ViewSupport (theCSSFramework)

-- | Render breadcrumbs.
renderBreadcrumbs :: (?context::ControllerContext) => [ BreadcrumbsItem ] -> Html
renderBreadcrumbs breadcrumbsItems = [hsx|
    {forEach breadcrumbsItems (renderBreadcrumb breadcrumbsItems)}
|]


renderBreadcrumb :: (?context::ControllerContext) => [ BreadcrumbsItem ] -> BreadcrumbsItem -> Html
renderBreadcrumb breadcrumbsItems breadcrumbsItem =
    styleBreadcrumbsItem theCSSFramework theCSSFramework breadcrumbsItems breadcrumbsItem
