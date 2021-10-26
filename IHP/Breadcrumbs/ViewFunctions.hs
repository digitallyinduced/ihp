module IHP.Breadcrumbs.ViewFunctions (
    module IHP.Breadcrumbs.Types,
    renderBreadcrumbs,
) where

import IHP.Prelude
import IHP.Breadcrumbs.Types

import Text.Blaze.Html (Html)
import IHP.HSX.QQ (hsx)


import IHP.View.Classes
import IHP.View.Types (styleHomepageBreadcrumbsItem, styleBreadcrumbsItem)
import IHP.View.CSSFramework

-- | Render breadcrumbs.
renderBreadcrumbs :: [ BreadcrumbsItem ] -> Bool -> Html
renderBreadcrumbs breadcrumbsItems showFirstItemAsLinkToHomepage = [hsx|
    {forEach breadcrumbsItems renderBreadcrumb}
|]


renderBreadcrumb :: BreadcrumbsItem -> Html
renderBreadcrumb breadcrumbsItem =
    mempty
