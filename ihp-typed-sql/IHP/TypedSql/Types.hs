module IHP.TypedSql.Types
    ( TypedQuery (..)
    ) where

import qualified Hasql.Decoders                as HasqlDecoders
import qualified Hasql.DynamicStatements.Snippet as Snippet

-- | Prepared query with a custom row parser.
-- High-level: this is the runtime value produced by the typed SQL quasiquoter.
data TypedQuery result = TypedQuery
    { tqSnippet       :: !Snippet.Snippet
    , tqResultDecoder :: !(HasqlDecoders.Row result)
    }
