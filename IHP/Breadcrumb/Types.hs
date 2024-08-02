module IHP.Breadcrumb.Types where

import IHP.Prelude
import IHP.HSX.Html (Html)
import IHP.HSX.Html (renderHtml)
import ClassyPrelude

data BreadcrumbItem =
    BreadcrumbItem
    { breadcrumbLabel :: Html -- ^ The label of a single breadcrumbs item. May be HTML, thus use SVG or font icon.
    , url :: Maybe Text -- ^ The URL of the item. If Nothing, it will only show the label.
    }

-- Html doesn't have a Show instance, so we define it manually and use 'renderHtml' for that.
instance Show BreadcrumbItem where
    show breadcrumbItem = [plain|{ breadcrumbLabel = "#{breadcrumbLabel}", url = #{url} }|]
        where
            breadcrumbLabel = renderHtml $ breadcrumbItem.breadcrumbLabel
            url = ClassyPrelude.show $ breadcrumbItem.url
