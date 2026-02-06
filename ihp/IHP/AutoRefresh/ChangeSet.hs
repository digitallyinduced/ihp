{-|
Module: IHP.AutoRefresh.ChangeSet
Description: Helpers for fine-grained Auto Refresh

Re-exports the user-facing types and helper functions used by 'IHP.AutoRefresh.autoRefreshWith'.
-}
module IHP.AutoRefresh.ChangeSet
    ( AutoRefreshOperation (..)
    , AutoRefreshRowChange (..)
    , AutoRefreshChangeSet (..)
    , changesForTable
    , anyChangeOnTable
    , anyChangeWithField
    , rowFieldNew
    , rowFieldOld
    ) where

import IHP.AutoRefresh.Types
