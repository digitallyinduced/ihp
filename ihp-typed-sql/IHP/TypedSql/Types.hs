module IHP.TypedSql.Types
    ( QueryCardinality (..)
    , TypedQuery (..)
    , TypedQueryResult
    ) where

import           Data.Kind                       (Type)
import qualified Hasql.Decoders                as HasqlDecoders
import qualified Hasql.DynamicStatements.Snippet as Snippet
import           Prelude                         (Eq, Maybe, Show)

-- | What the SQL parser can prove about how many rows a query returns.
data QueryCardinality
    = ManyRows
    | AtMostOneRow
    | ExactlyOneRow
    deriving (Eq, Show)

-- | The result shape returned by 'IHP.TypedSql.sqlQueryTyped'.
type family TypedQueryResult (cardinality :: QueryCardinality) result :: Type where
    TypedQueryResult 'ManyRows result = [result]
    TypedQueryResult 'AtMostOneRow result = Maybe result
    TypedQueryResult 'ExactlyOneRow result = result

-- | Prepared query with a custom row parser.
-- High-level: this is the runtime value produced by the typed SQL quasiquoter.
data TypedQuery (cardinality :: QueryCardinality) result = TypedQuery
    { tqSnippet       :: !Snippet.Snippet
    , tqResultDecoder :: !(HasqlDecoders.Row result)
    }
