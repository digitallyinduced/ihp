module IHP.Breadcrumbs.Types where

import IHP.Prelude
import Text.Blaze.Html (Html)

data BreadcrumbsItem =
    BreadcrumbsItem
    { label :: Html -- The label of a single breadcrumbs item. May be HTML, thus use SVG or font icon.
    , url :: Maybe Text -- The URL of the item. If Nothing, it will only show the label.
    }
    -- @todo: Can't due to label :: Html
    -- deriving(Show)
