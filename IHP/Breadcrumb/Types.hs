module IHP.Breadcrumb.Types where

import IHP.Prelude
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.String (renderHtml)
import ClassyPrelude

data BreadcrumbItem =
    BreadcrumbItem
    { label :: Html -- ^ The label of a single breadcrumbs item. May be HTML, thus use SVG or font icon.
    , url :: Maybe Text -- ^ The URL of the item. If Nothing, it will only show the label.
    }

-- Html doesn't have a Show instance, so we define it manually and use 'renderHtml' for that.
instance Show BreadcrumbItem where
    show breadcrumbItem = [plain|{ label = "#{label}", url = #{url} }|]
        where
            label = renderHtml $ get #label breadcrumbItem
            url = ClassyPrelude.show $ get #url breadcrumbItem
