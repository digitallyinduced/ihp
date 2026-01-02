{-# LANGUAGE ImplicitParams #-}

module IHP.Postgres.Typed
    ( pgSQL
    , pgQuery
    , pgQueryOne
    ) where

import           IHP.ModelSupport          (ModelContext)
import           IHP.Prelude
import           IHP.TypedSql              (TypedQuery, runTyped, runTypedOne,
                                            typedSql)
import           Language.Haskell.TH.Quote (QuasiQuoter)

pgSQL :: QuasiQuoter
pgSQL = typedSql

pgQuery :: (?modelContext :: ModelContext) => TypedQuery result -> IO [result]
pgQuery = runTyped

pgQueryOne :: (?modelContext :: ModelContext) => TypedQuery result -> IO result
pgQueryOne = runTypedOne
