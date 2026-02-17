module IHP.Pagination.Types where

import Prelude
import Data.Text (Text)
import IHP.HaskellSupport (SetField(..))

data Pagination =
    Pagination
    {
        pageSize :: Int     -- the number of items per page
    ,   totalItems :: Int   -- the total number of items in the result
    ,   currentPage :: Int  -- the currently-selected page
    ,   window :: Int       -- The size of the window
    ,   paramSuffix :: Text -- ^ Suffix for query param names to support multiple paginations per page.
                            -- First pagination gets @""@, second gets @"_2"@, third @"_3"@, etc.
    }
    deriving(Show)

-- | Options for customizing a pagination, to be used with 'paginateOptions'.
data Options = 
    Options 
        {
            maxItems :: Int -- ^ The maximum items per page. Default 50.
        ,   windowSize :: Int -- ^ The size of the window in the page selector. Default 5.
        }

instance SetField "maxItems" Options Int where
    setField value options =
        options { maxItems = value }
instance SetField "windowSize" Options Int where
    setField value options =
        options { windowSize = value }

data PageDotDot =
      Page Int
    | DotDot Int