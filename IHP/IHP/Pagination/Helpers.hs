module IHP.Pagination.Helpers where

import IHP.Prelude
import IHP.Pagination.Types

-- | Gets the number of the last page from a 'Pagination' state. Uses the total
-- number of items and the page size to calculate the final page.
getLastPage :: Pagination -> Int
getLastPage Pagination { pageSize, totalItems } =
    if pageSize > 1 then
        ((totalItems - 1) `div` pageSize) + 1
    else
        totalItems `div` pageSize

-- | Returns 'True' if there is a next page in the pagination, 'False' otherwise.
hasNextPage :: Pagination -> Bool
hasNextPage pagination@Pagination { currentPage } =
    getLastPage pagination > currentPage

-- | Returns 'True' if there is a previous page in the pagination, 'False' otherwise.
hasPreviousPage :: Pagination -> Bool
hasPreviousPage pagination@Pagination { currentPage } =
    currentPage > 1