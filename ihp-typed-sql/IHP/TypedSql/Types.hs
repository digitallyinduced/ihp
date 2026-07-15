module IHP.TypedSql.Types
    ( QueryCardinality (..)
    , QueryExecResult (..)
    , TypedQuery (..)
    , TypedQueryResult
    , SqlExecTypedResult
    ) where

import           Data.Int                        (Int64)
import           Data.Kind                       (Type)
import           GHC.TypeLits                    (ErrorMessage (Text), TypeError)
import qualified Hasql.Decoders                as HasqlDecoders
import qualified Hasql.DynamicStatements.Snippet as Snippet
import           Prelude                         (Bool, Eq, Maybe, Show)
import           Data.Text                       (Text)

-- | What the SQL parser can prove about how many rows a query returns.
data QueryCardinality
    = ManyRows
    | AtMostOneRow
    | ExactlyOneRow
    deriving (Eq, Show)

-- | What kind of result an SQL statement has.
--
-- This marker is separate from 'QueryCardinality' because statements without
-- result columns can still have different execution results: DML statements
-- report affected rows, while utility statements such as @SET CONSTRAINTS@ do
-- not report a row count.
data QueryExecResult
    = ReturnsRows
    | ReturnsAffectedRows
    | ReturnsNoResult
    deriving (Eq, Show)

-- | The result shape returned by 'IHP.TypedSql.sqlQueryTyped'.
type family TypedQueryResult (cardinality :: QueryCardinality) result :: Type where
    TypedQueryResult 'ManyRows result = [result]
    TypedQueryResult 'AtMostOneRow result = Maybe result
    TypedQueryResult 'ExactlyOneRow result = result

-- | The result returned by 'IHP.TypedSql.sqlExecTyped'.
type family SqlExecTypedResult (execResult :: QueryExecResult) :: Type where
    SqlExecTypedResult 'ReturnsAffectedRows = Int64
    SqlExecTypedResult 'ReturnsNoResult = ()
    SqlExecTypedResult 'ReturnsRows = TypeError ('Text "sqlExecTyped cannot run SQL statements that return rows. Use sqlQueryTyped instead.")

-- | Prepared query with a custom row parser.
-- High-level: this is the runtime value produced by the typed SQL quasiquoter.
data TypedQuery (cardinality :: QueryCardinality) (execResult :: QueryExecResult) result = TypedQuery
    { tqSnippet       :: !Snippet.Snippet
    , tqResultDecoder :: !(HasqlDecoders.Row result)
    -- | Tables read by the query and, when exposed by the result, the output
    -- column containing that table's conventional single-column @id@ key.
    -- AutoRefresh uses this runtime metadata to retain the parameterized query.
    , tqAutoRefreshTables :: ![(Text, Maybe Text)]
    -- | Whether the parser proved that each output row depends only on the
    -- corresponding physical row. Complex shapes fall back to table tracking.
    , tqAutoRefreshRowMatchingSafe :: !Bool
    }
