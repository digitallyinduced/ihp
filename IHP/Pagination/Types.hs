module IHP.Pagination.Types where

import IHP.Prelude
import IHP.QueryBuilder

data Pagination = 
    Pagination
    {
        pageSize :: Int
    ,   totalItems :: Int
    ,   currentPage :: Int
    ,   window :: Int
    }
    deriving(Show)

-- | Options for customizing a pagination, to be used with 'paginateOptions'.
data Options = 
    Options 
        {
            maxItemsOption :: Int -- ^ The maximum items per page
        ,   windowSizeOption :: Int
        }

data PageDotDot =
      Page Int
    | DotDot Int