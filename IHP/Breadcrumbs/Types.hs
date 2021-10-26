module IHP.Breadcrumbs.Types where

import IHP.Prelude

data BreadcrumbsItem =
    BreadcrumbsItem
    { label :: Html -- The label of the breadcrumb. May be HTML, thus use SVG or font icon.
    , url :: Maybe Text -- The URL to link to. If Nothing, it will only show the label.
    }
    deriving(Show)
