module IHP.Breadcrumbs.Types where

import IHP.Prelude
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.String (renderHtml)
import ClassyPrelude

data BreadcrumbsItem =
    BreadcrumbsItem
    { label :: Html -- The label of a single breadcrumbs item. May be HTML, thus use SVG or font icon.
    , url :: Maybe Text -- The URL of the item. If Nothing, it will only show the label.
    , isActive :: Bool -- Determine if the breadcrumbs item should be marked as active.
    }

-- Html doesn't have a Show instance, so we define it manually and use 'renderHtml' for that.
instance Show BreadcrumbsItem where
    show breadcrumbsItem =
        [plain|
            { label = "#{label}"
            , url = #{url}
            , isActive = #{isActive}
            }
        |]
        where
            label = renderHtml $ get #label breadcrumbsItem
            url = ClassyPrelude.show $ get #url breadcrumbsItem
            isActive = ClassyPrelude.show $ get #isActive breadcrumbsItem
