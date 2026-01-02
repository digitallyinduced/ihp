{-# LANGUAGE ImplicitParams #-}

module IHP.Postgres.Typed
    ( pgSQL
    , pgQuery
    ) where

import           IHP.ModelSupport          (ModelContext)
import           IHP.Prelude
import           IHP.TypedSql              (TypedQuery, sqlQueryTyped, typedSql)
import           Language.Haskell.TH.Quote (QuasiQuoter)

pgSQL :: QuasiQuoter
pgSQL = typedSql

pgQuery :: (?modelContext :: ModelContext) => TypedQuery result -> IO [result]
pgQuery = sqlQueryTyped
