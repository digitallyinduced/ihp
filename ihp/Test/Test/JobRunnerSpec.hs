module Test.JobRunnerSpec where

import Test.Hspec
import IHP.Prelude
import IHP.ModelSupport
import IHP.Hasql.FromRow (FromRowHasql (..), HasqlDecodeColumn (..))
import IHP.Job.Types
import IHP.Job.Runner.WorkerLoop (jobWorkerFetchAndRunLoop)
import qualified IHP.Job.Queue as Queue
import qualified IHP.Job.Queue.Result as Result
import qualified IHP.Job.Queue.Worker as Worker
import qualified IHP.FrameworkConfig as Config
import IHP.Environment (Environment (Development))
import qualified IHP.PGListener as PGListener
import qualified Hasql.Pool as Pool
import qualified Hasql.Session as Session
import qualified Hasql.Statement as Statement
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Async as Async
import qualified Control.Exception as Exception
import Control.Concurrent.STM (atomically, writeTVar, readTVarIO)
import Control.Monad.Trans.Resource (runResourceT, release)
import System.Environment (lookupEnv)
import System.Timeout (timeout)

data RunnerJob = RunnerJob
    { id :: Id' "job_runner_spec_jobs"
    , runAt :: UTCTime
    , attemptsCount :: Int
    , lockedBy :: Maybe UUID
    , lockedAt :: Maybe UTCTime
    } deriving (Show, Eq)

type instance GetTableName RunnerJob = "job_runner_spec_jobs"
type instance GetModelByTableName "job_runner_spec_jobs" = RunnerJob
type instance PrimaryKey "job_runner_spec_jobs" = UUID

instance Table RunnerJob where
    columnNames = ["id", "run_at", "attempts_count", "locked_by", "locked_at"]
    primaryKeyColumnNames = ["id"]

instance FromRowHasql RunnerJob where
    hasqlRowDecoder = RunnerJob <$> hasqlColumnDecoder <*> hasqlColumnDecoder
        <*> hasqlColumnDecoder <*> hasqlColumnDecoder <*> hasqlColumnDecoder

instance Job RunnerJob where
    perform _ = waitForFinish
        where
            waitForFinish = do
                done <- queryBool ?modelContext.hasqlPool
                    "SELECT bool_and(should_finish) FROM job_runner_spec_jobs"
                unless done (Concurrent.threadDelay 10000 >> waitForFinish)
    maxConcurrency = 1
    timeoutInMicroseconds = Just 2000000

tests :: Spec
tests = describe "IHP.Job.Runner ownership" do
    it "cancels running work and immediately refunds its attempt" $
        withFixture \modelContext config databaseUrl -> do
            let pool = modelContext.hasqlPool
            withRunner modelContext config databaseUrl \process -> do
                waitFor pool "status = 'job_status_running'"
                timeout 1000000 (release (fst process.dispatcher)) `shouldReturn` Just ()
                readTVarIO process.activeCount `shouldReturn` 0
                queryBool pool
                    "SELECT status = 'job_status_retry' AND locked_by IS NULL AND locked_at IS NULL AND attempts_count = 0 AND run_at <= NOW() FROM job_runner_spec_jobs"
                    `shouldReturn` True

    it "drains the current job without claiming the next job" $
        withFixture \modelContext config databaseUrl -> do
            let pool = modelContext.hasqlPool
            withRunner modelContext config databaseUrl \process -> do
                waitFor pool "status = 'job_status_running'"
                atomically $ writeTVar process.isStopping True
                script pool
                    "INSERT INTO job_runner_spec_jobs (id) VALUES ('10000000-0000-0000-0000-000000000002'); UPDATE job_runner_spec_jobs SET should_finish = TRUE"
                timeout 1000000 (Async.wait (snd process.dispatcher)) `shouldReturn` Just ()
                queryBool pool
                    "SELECT count(*) FILTER (WHERE status = 'job_status_succeeded') = 1 AND count(*) FILTER (WHERE status = 'job_status_not_started' AND attempts_count = 0) = 1 FROM job_runner_spec_jobs"
                    `shouldReturn` True

    it "counts a real timeout as a failed attempt, not a worker interruption" $
        withFixture \modelContext config databaseUrl -> do
            let pool = modelContext.hasqlPool
            withRunner modelContext config databaseUrl \_ -> do
                waitFor pool "status = 'job_status_retry' AND last_error = 'Timeout reached' AND attempts_count = 1 AND run_at > NOW()"

    it "propagates claim failures to the worker supervisor" $
        withFixture \modelContext config databaseUrl -> do
            let pool = modelContext.hasqlPool
            script pool "UPDATE job_runner_spec_jobs SET should_finish = TRUE"
            outcome <- Exception.try @Async.ExceptionInLinkedThread $
                withRunner modelContext config databaseUrl \process -> do
                    waitFor pool "status = 'job_status_succeeded'"
                    script pool "DROP TABLE job_runner_spec_jobs"
                    _ <- atomically $ Queue.tryWriteTBQueue process.action JobAvailable
                    Concurrent.threadDelay 2000000
            case outcome of
                Left _ -> pure ()
                Right () -> expectationFailure "claim failure did not reach the worker supervisor"

    it "rejects late results from an earlier claim by the same worker" $
        withFixture \modelContext config _ -> do
            let pool = modelContext.hasqlPool
            let ?context = config
            Just oldJob <- Queue.fetchNextJob @RunnerJob pool testWorkerId
            Result.jobDidInterrupt pool oldJob
            Just currentJob <- Queue.fetchNextJob @RunnerJob pool testWorkerId
            oldJob.lockedAt `shouldNotBe` currentJob.lockedAt
            Result.jobDidSucceed pool oldJob
            Result.jobDidTimeout pool oldJob
            Result.jobDidFail pool oldJob (Exception.toException (userError "late failure"))
            Result.jobDidInterrupt pool oldJob
            queryBool pool "SELECT status = 'job_status_running' AND attempts_count = 1 AND last_error IS NULL FROM job_runner_spec_jobs"
                `shouldReturn` True
            Result.jobDidSucceed pool currentJob
            queryBool pool "SELECT status = 'job_status_succeeded' FROM job_runner_spec_jobs" `shouldReturn` True

    it "does not recover an old job while its worker heartbeat is live" $
        withFixture \modelContext _ _ -> do
            let pool = modelContext.hasqlPool
            _ <- Queue.fetchNextJob @RunnerJob pool testWorkerId
            script pool "UPDATE job_runner_spec_jobs SET locked_at = NOW() - INTERVAL '2 days'"
            Queue.recoverStaleJobs @RunnerJob pool 1
            queryBool pool "SELECT status = 'job_status_running' FROM job_runner_spec_jobs" `shouldReturn` True

    it "rejects a departed worker's result after replacement claims the job" $
        withFixture \modelContext config _ -> do
            let pool = modelContext.hasqlPool
            let ?context = config
            let replacementId = "10000000-0000-0000-0000-000000000011"
            Just oldJob <- Queue.fetchNextJob @RunnerJob pool testWorkerId
            Worker.unregisterWorker pool testWorkerId
            Exception.bracket_ (Worker.registerWorker pool replacementId) (Worker.unregisterWorker pool replacementId) do
                Just replacementJob <- Queue.fetchNextJob @RunnerJob pool replacementId
                Result.jobDidSucceed pool oldJob
                Result.jobDidFail pool oldJob (Exception.toException (userError "departed worker"))
                Result.jobDidTimeout pool oldJob
                Result.jobDidInterrupt pool oldJob
                queryBool pool "SELECT status = 'job_status_running' AND locked_by = '10000000-0000-0000-0000-000000000011' AND attempts_count = 1 FROM job_runner_spec_jobs"
                    `shouldReturn` True
                Result.jobDidSucceed pool replacementJob

    it "does not let an expired worker claim another job" $
        withFixture \modelContext _ _ -> do
            let pool = modelContext.hasqlPool
            script pool "UPDATE public.ihp_job_workers SET heartbeat_at = NOW() - INTERVAL '3 minutes' WHERE id = '10000000-0000-0000-0000-000000000010'"
            Queue.fetchNextJob @RunnerJob pool testWorkerId `shouldReturn` Nothing

testWorkerId :: UUID
testWorkerId = "10000000-0000-0000-0000-000000000010"

withRunner :: ModelContext -> Config.FrameworkConfig -> ByteString -> (JobWorkerProcess -> IO ()) -> IO ()
withRunner modelContext config databaseUrl action =
    PGListener.withPGListener databaseUrl noopLogger \pgListener ->
        runResourceT do
            process <- jobWorkerFetchAndRunLoop @RunnerJob JobWorkerArgs
                { workerId = testWorkerId, modelContext, frameworkConfig = config, pgListener }
            liftIO $ action process `Exception.finally` PGListener.unsubscribe process.subscription pgListener

withFixture :: (ModelContext -> Config.FrameworkConfig -> ByteString -> IO ()) -> IO ()
withFixture action = do
    databaseUrl <- maybe "postgresql:///postgres" cs <$> lookupEnv "DATABASE_URL"
    Exception.bracket (createModelContext databaseUrl noopLogger) releaseModelContext \modelContext -> do
        let pool = modelContext.hasqlPool
        let cleanup = do
                script pool "DROP TABLE IF EXISTS job_runner_spec_jobs CASCADE; DROP FUNCTION IF EXISTS notify_job_queued_job_runner_spec_jobs() CASCADE"
                Worker.unregisterWorker pool testWorkerId
        let setup = do
                Worker.ensureWorkerRegistry pool
                cleanup
                script pool
                    "DO $$ BEGIN CREATE TYPE public.job_status AS ENUM ('job_status_not_started', 'job_status_running', 'job_status_failed', 'job_status_timed_out', 'job_status_succeeded', 'job_status_retry'); EXCEPTION WHEN duplicate_object THEN NULL; END $$;\
                    \CREATE TABLE job_runner_spec_jobs (id UUID PRIMARY KEY, status public.job_status NOT NULL DEFAULT 'job_status_not_started', created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(), updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(), run_at TIMESTAMPTZ NOT NULL DEFAULT NOW(), locked_at TIMESTAMPTZ, locked_by UUID, attempts_count INT NOT NULL DEFAULT 0, last_error TEXT, should_finish BOOL NOT NULL DEFAULT FALSE);\
                    \INSERT INTO job_runner_spec_jobs (id) VALUES ('10000000-0000-0000-0000-000000000001')"
                Worker.registerWorker pool testWorkerId
                Worker.ensureJobWorkerForeignKey pool "job_runner_spec_jobs"
        result <- Exception.try $ Exception.bracket_ setup cleanup do
            config <- Config.buildFrameworkConfig noopLogger (Config.option Development)
            action modelContext config databaseUrl
        case result of
            Right () -> pure ()
            Left (HasqlError (Pool.ConnectionUsageError _)) -> pendingWith "PostgreSQL unavailable"
            Left exception -> Exception.throwIO exception

script :: Pool.Pool -> Text -> IO ()
script pool sql = Queue.runPool pool (Session.script sql)

queryBool :: Pool.Pool -> Text -> IO Bool
queryBool pool sql = Queue.runPool pool $ Session.statement () $
    Statement.unpreparable sql Encoders.noParams (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))

waitFor :: Pool.Pool -> Text -> IO ()
waitFor pool condition = do
    result <- timeout 5000000 loop
    result `shouldBe` Just ()
    where
        loop = do
            ready <- queryBool pool ("SELECT bool_and(" <> condition <> ") FROM job_runner_spec_jobs")
            unless ready (Concurrent.threadDelay 10000 >> loop)
