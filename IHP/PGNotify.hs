{-|
Module: IHP.PGNotify
Description: Be notified about database changes
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.PGNotify where

import IHP.Prelude
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Types as PG
import qualified Database.PostgreSQL.Simple.Notification as PG
import Control.Concurrent.Async
import IHP.ModelSupport

watchInsertOrUpdateTable :: (?modelContext :: ModelContext) => Text -> IO () -> IO (Async ())
watchInsertOrUpdateTable tableName onInsertOrUpdate = do
    let ModelContext { databaseConnection } = ?modelContext
    PG.execute databaseConnection (PG.Query $ cs $ createNotificationTrigger tableName) ()

    let listenStatement = "LISTEN " <> PG.Query (cs $ eventName tableName)
    async do
        forever do
            PG.execute databaseConnection listenStatement ()
            notification <- PG.getNotification databaseConnection
            onInsertOrUpdate
    


createNotificationTrigger :: Text -> Text
createNotificationTrigger tableName = "CREATE OR REPLACE FUNCTION " <> functionName <> "() RETURNS TRIGGER AS $$"
        <> "BEGIN\n"
        <> "    PERFORM pg_notify('" <> eventName tableName <> "', row_to_json(new)::text);\n"
        <> "    RETURN new;"
        <> "END;\n"
        <> "$$ language plpgsql;"
        <> "DROP TRIGGER IF EXISTS " <> insertTriggerName <> " ON " <> tableName <> "; CREATE TRIGGER " <> insertTriggerName <> " AFTER INSERT ON \"" <> tableName <> "\" FOR EACH ROW EXECUTE PROCEDURE " <> functionName <> "();\n"
        <> "DROP TRIGGER IF EXISTS " <> updateTriggerName <> " ON " <> tableName <> "; CREATE TRIGGER " <> updateTriggerName <> " AFTER UPDATE ON \"" <> tableName <> "\" FOR EACH ROW EXECUTE PROCEDURE " <> functionName <> "();\n"
    where
        functionName = "notify_did_change_" <> tableName
        insertTriggerName = "did_insert_" <> tableName
        updateTriggerName = "did_update_" <> tableName

-- | Retuns the event name of the event that the pg notify trigger dispatches
eventName :: Text -> Text
eventName tableName = "did_change_" <> tableName
