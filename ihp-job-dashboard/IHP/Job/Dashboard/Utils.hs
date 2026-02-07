module IHP.Job.Dashboard.Utils where

import IHP.Prelude
import IHP.ModelSupport
import qualified Hasql.Decoders as Decoders
import qualified Hasql.DynamicStatements.Snippet as Snippet
import IHP.Job.Dashboard.Types (sqlIdentifier)

numberOfPagesForTable :: (?modelContext::ModelContext) => Text -> Int -> IO Int
numberOfPagesForTable table pageSize = do
    totalRecords <- totalRecordsForTable table
    pure $ case totalRecords `quotRem` pageSize of
        (pages, 0) -> pages
        (pages, _) -> pages + 1

totalRecordsForTable :: (?modelContext :: ModelContext) => Text -> IO Int
totalRecordsForTable table = withHasqlOrPgSimple
    (\pool -> fromIntegral <$> sqlQueryHasql pool
        (Snippet.sql "SELECT COUNT(*) FROM " <> sqlIdentifier table)
        (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8))))
    (sqlQueryScalar (cs $ "SELECT COUNT(*) FROM " <> table) ())
