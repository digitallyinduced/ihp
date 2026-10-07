-- | Process-level leases for job ownership. A worker id is never reused.
module IHP.Job.Queue.Worker
( ensureWorkerRegistry
, registerWorker
, heartbeatWorker
, unregisterWorker
, reapExpiredWorkers
, ensureJobWorkerForeignKey
) where

import IHP.Prelude
import IHP.Job.Queue.Pool (runPool)
import qualified Hasql.Pool as Pool
import qualified Hasql.Session as Session
import qualified Hasql.Statement as Statement
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Decoders as Decoders
import qualified Data.Text as Text

-- | Install framework-owned infrastructure. This does not modify application
-- schemas until 'ensureJobWorkerForeignKey' is called for a job table.
ensureWorkerRegistry :: Pool.Pool -> IO ()
ensureWorkerRegistry pool = runPool pool $ Session.script $
    "DO $install$ BEGIN "
    <> "PERFORM set_config('lock_timeout', '5s', true);"
    <> "PERFORM pg_advisory_xact_lock(hashtext('ihp_job_workers'), 0);"
    <> "IF to_regclass('public.ihp_job_workers') IS NULL THEN "
    <> "CREATE TABLE public.ihp_job_workers ("
    <> "id UUID PRIMARY KEY, started_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),"
    <> "heartbeat_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp());"
    <> "END IF;"
    <> "END $install$;"
    <> "CREATE OR REPLACE FUNCTION public.ihp_release_worker_job() RETURNS trigger AS $release$ "
    <> "BEGIN "
    -- Only a worker deletion may release a still-running job this way. Normal
    -- completion changes status itself, and manual unlocks of live owners do not
    -- masquerade as worker crashes.
    <> "IF OLD.status = 'job_status_running' AND NEW.status = 'job_status_running'"
    <> " AND OLD.locked_by IS NOT NULL AND NEW.locked_by IS NULL"
    <> " AND NOT EXISTS (SELECT 1 FROM public.ihp_job_workers WHERE id = OLD.locked_by) THEN "
    <> "NEW.status := 'job_status_retry';"
    <> "NEW.locked_at := NULL;"
    <> "NEW.run_at := clock_timestamp();"
    <> "NEW.updated_at := clock_timestamp();"
    <> "NEW.attempts_count := greatest(0, OLD.attempts_count - 1);"
    <> "END IF; RETURN NEW; END $release$ LANGUAGE plpgsql;"

registerWorker :: Pool.Pool -> UUID -> IO ()
registerWorker pool workerId = runPool pool $ Session.statement workerId $
    Statement.unpreparable
        "INSERT INTO public.ihp_job_workers (id) VALUES ($1)"
        uuidEncoder Decoders.noResult

-- | An expired lease cannot be revived, even if the reaper has not deleted it.
-- False requires the caller to stop taking and performing jobs.
heartbeatWorker :: Pool.Pool -> UUID -> IO Bool
heartbeatWorker pool workerId = do
    result <- runPool pool $ Session.statement workerId $
        Statement.unpreparable
            ("UPDATE public.ihp_job_workers SET heartbeat_at = clock_timestamp()"
                <> " WHERE id = $1 AND heartbeat_at > clock_timestamp() - interval '120 seconds'"
                <> " RETURNING true")
            uuidEncoder (Decoders.rowMaybe (Decoders.column (Decoders.nonNullable Decoders.bool)))
    pure (fromMaybe False result)

-- | Call only after the worker's executions have stopped. The foreign keys
-- release remaining jobs in the same transaction as deleting the worker.
unregisterWorker :: Pool.Pool -> UUID -> IO ()
unregisterWorker pool workerId = runPool pool $ Session.statement workerId $
    Statement.unpreparable "DELETE FROM public.ihp_job_workers WHERE id = $1" uuidEncoder Decoders.noResult

reapExpiredWorkers :: Pool.Pool -> IO ()
reapExpiredWorkers pool = runPool pool $ Session.script
    -- The script is one implicit transaction: settings are local and an error
    -- rolls the deletion back. FK actions may otherwise wait indefinitely for a
    -- locked job table, preventing this process from renewing its own lease.
    ("SET LOCAL lock_timeout = '1s';"
        <> "SET LOCAL statement_timeout = '10s';"
        <> "DELETE FROM public.ihp_job_workers WHERE heartbeat_at <= clock_timestamp() - interval '120 seconds';")

-- | Install a validated ownership FK and release trigger once per job table.
-- Existing unregistered owners are an upgrade error, not proof of a crash.
-- Healthy tables require catalog reads only, avoiding DDL locks on every deploy.
ensureJobWorkerForeignKey :: Pool.Pool -> Text -> IO ()
ensureJobWorkerForeignKey pool tableName = runPool pool $ Session.script $
    "DO $install$ DECLARE job_table regclass := " <> quoteLiteral tableName <> "::regclass;"
    <> " has_orphans boolean; BEGIN "
    <> "PERFORM set_config('lock_timeout', '5s', true);"
    <> "PERFORM pg_advisory_xact_lock(hashtext('ihp_job_worker_fk'), job_table::oid::int);"
    <> "IF NOT EXISTS (SELECT 1 FROM pg_constraint WHERE conrelid = job_table AND conname = 'ihp_job_worker_fk') THEN "
    <> "EXECUTE format('LOCK TABLE %s IN SHARE ROW EXCLUSIVE MODE', job_table);"
    <> "EXECUTE format('SELECT EXISTS (SELECT 1 FROM %s j"
    <> " WHERE j.locked_by IS NOT NULL AND NOT EXISTS (SELECT 1 FROM public.ihp_job_workers w WHERE w.id = j.locked_by))', job_table) INTO has_orphans;"
    <> "IF has_orphans THEN "
    <> "RAISE EXCEPTION 'Cannot install IHP worker ownership: stop all old workers and release their orphaned job locks first. See the jobs guide upgrade instructions.';"
    <> "END IF;"
    <> "EXECUTE format('ALTER TABLE %s ADD CONSTRAINT ihp_job_worker_fk FOREIGN KEY (locked_by)"
    <> " REFERENCES public.ihp_job_workers(id) ON DELETE SET NULL', job_table);"
    <> "END IF;"
    -- Foreign-key actions otherwise scan the entire job history on every
    -- worker shutdown. Reuse a suitable application-owned index if available.
    <> "IF NOT EXISTS (SELECT 1 FROM pg_index i JOIN pg_attribute a"
    <> " ON a.attrelid = i.indrelid AND a.attnum = i.indkey[0]"
    <> " WHERE i.indrelid = job_table AND i.indisvalid AND a.attname = 'locked_by'"
    <> " AND (i.indpred IS NULL OR pg_get_expr(i.indpred, i.indrelid) = '(locked_by IS NOT NULL)')) THEN "
    <> "EXECUTE format('CREATE INDEX %I ON %s (locked_by) WHERE locked_by IS NOT NULL',"
    <> " 'ihp_job_worker_' || job_table::oid::text, job_table);"
    <> "END IF;"
    <> "IF NOT EXISTS (SELECT 1 FROM pg_trigger WHERE tgrelid = job_table AND tgname = 'ihp_release_worker_job' AND NOT tgisinternal) THEN "
    <> "EXECUTE format('CREATE TRIGGER ihp_release_worker_job BEFORE UPDATE ON %s"
    <> " FOR EACH ROW EXECUTE FUNCTION public.ihp_release_worker_job()', job_table);"
    <> "END IF; END $install$;"

uuidEncoder :: Encoders.Params UUID
uuidEncoder = Encoders.param (Encoders.nonNullable Encoders.uuid)

quoteLiteral :: Text -> Text
quoteLiteral value = "'" <> Text.replace "'" "''" value <> "'"
