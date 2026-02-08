module IHP.TypedSql.Types
    ( TypedQuery (..)
    ) where

import qualified Database.PostgreSQL.Simple         as PG
import qualified Database.PostgreSQL.Simple.FromRow as PGFR
import qualified Database.PostgreSQL.Simple.ToField as PGTF

-- | Prepared query with a custom row parser.
-- High-level: this is the runtime value produced by the typed SQL quasiquoter.
data TypedQuery result = TypedQuery
    { tqQuery     :: !PG.Query
    , tqParams    :: ![PGTF.Action]
    , tqRowParser :: !(PGFR.RowParser result)
    }
