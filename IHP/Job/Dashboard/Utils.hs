module IHP.Job.Dashboard.Utils where

import IHP.Prelude
import IHP.ModelSupport
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Types as PG
import qualified Database.PostgreSQL.Simple.FromField as PG
import qualified Database.PostgreSQL.Simple.ToField as PG

numberOfPagesForTable :: (?modelContext::ModelContext) => Text -> Int -> IO Int
numberOfPagesForTable table pageSize = do
    (PG.Only totalRecords : _) <- sqlQuery
        (PG.Query $ cs $ "SELECT COUNT(*) FROM " <> table)
        ()
    pure $ case totalRecords `quotRem` pageSize of
        (pages, 0) -> pages
        (pages, _) -> pages + 1

