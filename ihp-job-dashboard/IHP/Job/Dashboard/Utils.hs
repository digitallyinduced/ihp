module IHP.Job.Dashboard.Utils where

import IHP.Prelude
import IHP.ModelSupport
import qualified Hasql.Decoders as Decoders
import qualified Hasql.DynamicStatements.Snippet as Snippet

numberOfPagesForTable :: (?modelContext::ModelContext) => Text -> Int -> IO Int
numberOfPagesForTable table pageSize = do
    totalRecords <- totalRecordsForTable table
    pure $ case totalRecords `quotRem` pageSize of
        (pages, 0) -> pages
        (pages, _) -> pages + 1

totalRecordsForTable :: (?modelContext :: ModelContext) => Text -> IO Int
totalRecordsForTable table = sqlQueryScalar (Snippet.sql $ cs $ "SELECT COUNT(*) FROM " <> table) (Decoders.singleRow (Decoders.column (Decoders.nonNullable (fromIntegral <$> Decoders.int8))))
