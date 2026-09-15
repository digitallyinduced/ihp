{-|
Module: IHP.Pagination.Internal
Description: Page-size, current-page and offset math shared between paginators
Copyright: (c) digitally induced GmbH, 2025

These helpers derive the effective page size, the current page, and the SQL
offset from the request's @page@ and @maxItems@ query parameters.

They live in their own module so that every paginator stays in lockstep:
'IHP.Pagination.ControllerFunctions' (the QueryBuilder and raw-SQL paginators)
and the typed-SQL paginator in @ihp-typed-sql@ all share the exact same limits,
the same 200-item cap, and the same offset math instead of reimplementing it.
-}
module IHP.Pagination.Internal
( pageSize'
, page
, offset'
) where

import IHP.Prelude
import IHP.Controller.Param (paramOrDefault)
import IHP.Pagination.Types (Options(..))
import Network.Wai (Request)

-- We limit the page size to a maximum of 200, to prevent users from
-- passing in query params with a value that could overload the
-- database (e.g. maxItems=100000)
pageSize' :: (?request :: Request) => Options -> Int
pageSize' options = min (max 1 $ paramOrDefault @Int (maxItems options) "maxItems") 200

-- Page and page size shouldn't be lower than 1.
page :: (?request :: Request) => Int
page = max 1 $ paramOrDefault @Int 1 "page"

offset' :: Int -> Int -> Int
offset' pageSize page = (page - 1) * pageSize
