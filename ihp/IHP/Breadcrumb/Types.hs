module IHP.Breadcrumb.Types where

import Prelude (Show(..), Maybe, (<>), ($))
import Data.Text (Text)
import IHP.HSX.Markup (Html)

data BreadcrumbItem =
    BreadcrumbItem
    { breadcrumbLabel :: Html -- ^ The label of a single breadcrumbs items. May be HTML, thus use SVG or font icon.
    , url :: Maybe Text -- ^ The URL of the item. If Nothing, it will only show the label.
    }

-- Markup doesn't have a Show instance, so we define it manually and use 'renderMarkup' for that.
instance Show BreadcrumbItem where
    show breadcrumbItem = "{ breadcrumbLabel = \"" <> renderedLabel <> "\", url = " <> renderedUrl <> " }"
        where
            renderedLabel = show $ breadcrumbLabel breadcrumbItem
            renderedUrl = show $ url breadcrumbItem
