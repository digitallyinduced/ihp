module IHP.Job.Dashboard.Utils where

import IHP.Prelude
import IHP.ModelSupport
import qualified Hasql.Decoders as Decoders
import qualified Hasql.DynamicStatements.Snippet as Snippet
import qualified Hasql.Pool as HasqlPool
import qualified Data.Text as Text

-- | Safely quote a SQL identifier (table name) by escaping double quotes.
sqlIdentifier :: Text -> Snippet.Snippet
sqlIdentifier name = Snippet.sql ("\"" <> Text.replace "\"" "\"\"" name <> "\"")

-- | Get the hasql pool from the model context.
getHasqlPool :: (?modelContext :: ModelContext) => HasqlPool.Pool
getHasqlPool = ?modelContext.hasqlPool

numberOfPagesForTable :: (?modelContext::ModelContext) => Text -> Int -> IO Int
numberOfPagesForTable table pageSize = do
    totalRecords <- totalRecordsForTable table
    pure $ case totalRecords `quotRem` pageSize of
        (pages, 0) -> pages
        (pages, _) -> pages + 1

totalRecordsForTable :: (?modelContext :: ModelContext) => Text -> IO Int
totalRecordsForTable table =
    fromIntegral <$> sqlQueryHasql getHasqlPool
        (Snippet.sql "SELECT COUNT(*) FROM " <> sqlIdentifier table)
        (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8)))
