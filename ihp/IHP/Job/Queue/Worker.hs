-- | Process-level leases for job ownership. A worker id is never reused.
module IHP.Job.Queue.Worker
( ensureWorkerRegistry
, withJobWorker
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
import qualified Data.UUID.V4 as UUID
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Async as Async
import qualified Control.Exception.Safe as Exception
import qualified System.Timeout as Timeout

-- | Run a manual queue consumer with a fresh, renewable worker lease.
-- Installs ownership infrastructure for every supplied job table before invoking
-- the callback. Keep all job execution inside the callback, including any scoped
-- child threads: returning or throwing releases unfinished jobs for retry.
-- Heartbeat or recovery failures abort the callback; do not catch asynchronous
-- exceptions inside it. The dedicated runner provides its own lease management.
withJobWorker :: Pool.Pool -> [Text] -> (UUID -> IO a) -> IO a
withJobWorker pool jobTables action = do
    ensureWorkerRegistry pool
    mapM_ (ensureJobWorkerForeignKey pool) jobTables
    workerId <- UUID.nextRandom
    Exception.bracket_ (registerWorker pool workerId) (unregisterWorker pool workerId) $
        Async.withAsync (renewLease workerId) \heartbeat -> do
            Async.link heartbeat
            Async.withAsync recoverWorkers \recovery -> do
                Async.link recovery
                action workerId
    where
        renewLease workerId = forever do
            Concurrent.threadDelay 30000000
            alive <- Timeout.timeout 10000000 (heartbeatWorker pool workerId)
            unless (alive == Just True) $
                Exception.throwString "Job worker heartbeat lost; stopping this worker"
        recoverWorkers = forever do
            reapExpiredWorkers pool
            Concurrent.threadDelay 30000000

-- | Install framework-owned infrastructure. This does not modify application
-- schemas until 'ensureJobWorkerForeignKey' is called for a job table.
ensureWorkerRegistry :: Pool.Pool -> IO ()
ensureWorkerRegistry pool = runPool pool $ Session.script $
    "DO $install$ BEGIN "
    <> "PERFORM set_config('lock_timeout', '5s', true);"
    <> "PERFORM pg_advisory_xact_lock(hashtext('job_workers'), 0);"
    <> "IF to_regclass('public.job_workers') IS NULL THEN "
    <> "CREATE TABLE public.job_workers ("
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
    <> " AND NOT EXISTS (SELECT 1 FROM public.job_workers WHERE id = OLD.locked_by) THEN "
    <> "NEW.status := 'job_status_retry';"
    <> "NEW.locked_at := NULL;"
    <> "NEW.run_at := clock_timestamp();"
    <> "NEW.updated_at := clock_timestamp();"
    <> "NEW.attempts_count := greatest(0, OLD.attempts_count - 1);"
    <> "END IF; RETURN NEW; END $release$ LANGUAGE plpgsql;"
    -- Applications often allow one pending job per key through a partial unique
    -- index over the not-started and retry states. Requeueing a running job then
    -- conflicts when another job for the same key is already pending. That job
    -- covers the work, so the released one fails instead of aborting the
    -- transaction that releases it.
    <> "CREATE OR REPLACE FUNCTION public.ihp_requeue_job(job_table regclass, job_id uuid, owner uuid,"
    <> " claimed_at timestamptz, next_run_at timestamptz, failure text, refund_attempt boolean)"
    <> " RETURNS boolean AS $requeue$ BEGIN "
    <> "EXECUTE format('UPDATE %s SET status = ''job_status_retry'', locked_by = NULL, locked_at = NULL,"
    <> " updated_at = clock_timestamp(), run_at = coalesce($4, clock_timestamp()),"
    <> " last_error = coalesce($5, last_error),"
    <> " attempts_count = CASE WHEN $6 THEN greatest(0, attempts_count - 1) ELSE attempts_count END"
    <> " WHERE id = $1 AND status = ''job_status_running'' AND locked_by = $2 AND locked_at = $3', job_table)"
    <> " USING job_id, owner, claimed_at, next_run_at, failure, refund_attempt;"
    <> "RETURN true;"
    <> "EXCEPTION WHEN unique_violation THEN "
    <> "EXECUTE format('UPDATE %s SET status = ''job_status_failed'', locked_by = NULL, locked_at = NULL,"
    <> " updated_at = clock_timestamp(), last_error = $4"
    <> " WHERE id = $1 AND status = ''job_status_running'' AND locked_by = $2 AND locked_at = $3', job_table)"
    <> " USING job_id, owner, claimed_at,"
    <> " concat_ws(' ', failure, 'Not retried: a pending job for the same work already exists.');"
    <> "RETURN false;"
    <> "END $requeue$ LANGUAGE plpgsql;"
    -- Release running jobs row by row before deleting the workers. Releasing
    -- them through the foreign key action instead lets one conflicting job abort
    -- the removal of every worker in the statement.
    <> "CREATE OR REPLACE FUNCTION public.ihp_remove_job_workers(worker_ids uuid[])"
    <> " RETURNS integer AS $remove$ DECLARE job_table regclass; job record; released integer := 0; BEGIN "
    <> "IF coalesce(cardinality(worker_ids), 0) = 0 THEN RETURN 0; END IF;"
    <> "PERFORM 1 FROM public.job_workers WHERE id = ANY (worker_ids) FOR UPDATE;"
    <> "FOR job_table IN SELECT conrelid::regclass FROM pg_constraint WHERE conname = 'ihp_job_worker_fk'"
    <> " AND contype = 'f' AND confrelid = 'public.job_workers'::regclass LOOP "
    <> "FOR job IN EXECUTE format('SELECT id, locked_by, locked_at FROM %s"
    <> " WHERE locked_by = ANY ($1) AND status = ''job_status_running''', job_table) USING worker_ids LOOP "
    <> "PERFORM public.ihp_requeue_job(job_table, job.id, job.locked_by, job.locked_at, NULL, NULL, true);"
    <> "released := released + 1;"
    <> "END LOOP; END LOOP;"
    <> "DELETE FROM public.job_workers WHERE id = ANY (worker_ids);"
    <> "RETURN released;"
    <> "END $remove$ LANGUAGE plpgsql;"

registerWorker :: Pool.Pool -> UUID -> IO ()
registerWorker pool workerId = runPool pool $ Session.statement workerId $
    Statement.unpreparable
        "INSERT INTO public.job_workers (id) VALUES ($1)"
        uuidEncoder Decoders.noResult

-- | An expired lease cannot be revived, even if the reaper has not deleted it.
-- False requires the caller to stop taking and performing jobs.
heartbeatWorker :: Pool.Pool -> UUID -> IO Bool
heartbeatWorker pool workerId = do
    result <- runPool pool $ Session.statement workerId $
        Statement.unpreparable
            ("UPDATE public.job_workers SET heartbeat_at = clock_timestamp()"
                <> " WHERE id = $1 AND heartbeat_at > clock_timestamp() - interval '120 seconds'"
                <> " RETURNING true")
            uuidEncoder (Decoders.rowMaybe (Decoders.column (Decoders.nonNullable Decoders.bool)))
    pure (fromMaybe False result)

-- | Call only after the worker's executions have stopped. Remaining jobs are
-- released in the same transaction as deleting the worker.
unregisterWorker :: Pool.Pool -> UUID -> IO ()
unregisterWorker pool workerId = runPool pool $ Session.statement workerId $
    Statement.unpreparable "SELECT public.ihp_remove_job_workers(ARRAY[$1])" uuidEncoder
        (fmap (const ()) (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4))))

reapExpiredWorkers :: Pool.Pool -> IO ()
reapExpiredWorkers pool = runPool pool $ Session.script
    -- The script is one implicit transaction: settings are local and an error
    -- rolls the deletion back. Releasing jobs may otherwise wait indefinitely for
    -- a locked job table, preventing this process from renewing its own lease.
    ("SET LOCAL lock_timeout = '1s';"
        <> "SET LOCAL statement_timeout = '10s';"
        <> "SELECT public.ihp_remove_job_workers(ARRAY(SELECT id FROM public.job_workers"
        <> " WHERE heartbeat_at <= clock_timestamp() - interval '120 seconds'));")

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
    <> " WHERE j.locked_by IS NOT NULL AND NOT EXISTS (SELECT 1 FROM public.job_workers w WHERE w.id = j.locked_by))', job_table) INTO has_orphans;"
    <> "IF has_orphans THEN "
    <> "RAISE EXCEPTION 'Cannot install IHP worker ownership: stop all old workers and release their orphaned job locks first. See the jobs guide upgrade instructions.';"
    <> "END IF;"
    <> "EXECUTE format('ALTER TABLE %s ADD CONSTRAINT ihp_job_worker_fk FOREIGN KEY (locked_by)"
    <> " REFERENCES public.job_workers(id) ON DELETE SET NULL', job_table);"
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
