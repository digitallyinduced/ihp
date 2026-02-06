{-# LANGUAGE ImplicitParams #-}

module IHP.Postgres.Typed
    ( pgSQL
    , pgQuery
    ) where

import           IHP.ModelSupport          (ModelContext)
import           IHP.Prelude
import           IHP.TypedSql              (TypedQuery, sqlQueryTyped, typedSql)
import           Language.Haskell.TH.Quote (QuasiQuoter)

