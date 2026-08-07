-- | Internal bridge between QueryBuilder fetches and AutoRefresh metadata.
module IHP.QueryBuilder.Tracking
    ( isQueryBuilderReadTrackingEnabled
    , trackQueryBuilderRead
    ) where

import IHP.Prelude
import Data.Dynamic (toDyn)
import IHP.QueryBuilder.Types (QueryBuilderRead)
import qualified IHP.ModelSupport.TableReadTracker as TableReadTracker

-- | Whether the current public table-read callback has a structured consumer.
isQueryBuilderReadTrackingEnabled :: (?modelContext :: ModelContext) => IO Bool
isQueryBuilderReadTrackingEnabled = case ?modelContext.trackTableReadCallback of
    Nothing -> pure False
    Just callback -> TableReadTracker.hasTrackedTableReadCallback callback

-- | Notify the unchanged public table tracker, then optionally attach the
-- structured QueryBuilder read for AutoRefresh's private consumer.
trackQueryBuilderRead :: (?modelContext :: ModelContext) => Text -> Maybe QueryBuilderRead -> IO ()
trackQueryBuilderRead tableName queryBuilderRead = case ?modelContext.trackTableReadCallback of
    Nothing -> pure ()
    Just callback -> do
        callback tableName
        forM_ queryBuilderRead \read ->
            TableReadTracker.trackStructuredTableRead callback tableName (toDyn read)
