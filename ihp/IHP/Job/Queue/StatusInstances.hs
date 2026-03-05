module IHP.Job.Queue.StatusInstances
( textToEnumJobStatusMap
, textToEnumJobStatus
) where

import IHP.Prelude
import IHP.Job.Types (JobStatus (..))
import qualified Database.PostgreSQL.Simple.FromField as PG
import qualified Database.PostgreSQL.Simple.ToField as PG
import IHP.ModelSupport (InputValue (..))
import qualified IHP.Controller.Param as Param
import qualified Data.HashMap.Strict as HashMap
import qualified Hasql.Encoders as Encoders
import Hasql.Implicits.Encoders (DefaultParamEncoder (..))

-- | Mapping for @JOB_STATUS@:
--
-- > CREATE TYPE JOB_STATUS AS ENUM ('job_status_not_started', 'job_status_running', 'job_status_failed', 'job_status_succeeded', 'job_status_retry');
--
-- These instances are needed by the generated @FromRow@ instances in user apps
-- (see 'compileFromRowInstance' in "IHP.SchemaCompiler").
instance PG.FromField JobStatus where
    fromField field (Just "job_status_not_started") = pure JobStatusNotStarted
    fromField field (Just "job_status_running") = pure JobStatusRunning
    fromField field (Just "job_status_failed") = pure JobStatusFailed
    fromField field (Just "job_status_timed_out") = pure JobStatusTimedOut
    fromField field (Just "job_status_succeeded") = pure JobStatusSucceeded
    fromField field (Just "job_status_retry") = pure JobStatusRetry
    fromField field (Just value) = PG.returnError PG.ConversionFailed field ("Unexpected value for enum value. Got: " <> cs value)
    fromField field Nothing = PG.returnError PG.UnexpectedNull field "Unexpected null for enum value"

-- The default state is @not started@
instance Default JobStatus where
    def = JobStatusNotStarted

-- | See 'FromField' instance above.
instance PG.ToField JobStatus where
    toField JobStatusNotStarted = PG.toField ("job_status_not_started" :: Text)
    toField JobStatusRunning = PG.toField ("job_status_running" :: Text)
    toField JobStatusFailed = PG.toField ("job_status_failed" :: Text)
    toField JobStatusTimedOut = PG.toField ("job_status_timed_out" :: Text)
    toField JobStatusSucceeded = PG.toField ("job_status_succeeded" :: Text)
    toField JobStatusRetry = PG.toField ("job_status_retry" :: Text)

instance InputValue JobStatus where
    inputValue JobStatusNotStarted = "job_status_not_started" :: Text
    inputValue JobStatusRunning = "job_status_running" :: Text
    inputValue JobStatusFailed = "job_status_failed" :: Text
    inputValue JobStatusTimedOut = "job_status_timed_out" :: Text
    inputValue JobStatusSucceeded = "job_status_succeeded" :: Text
    inputValue JobStatusRetry = "job_status_retry" :: Text

instance Param.ParamReader JobStatus where
    readParameter = Param.enumParamReader

-- | Parses a Text value to a JobStatus. Used by hasql decoders.
-- Uses HashMap for O(1) lookup.
textToEnumJobStatusMap :: HashMap.HashMap Text JobStatus
textToEnumJobStatusMap = HashMap.fromList
    [ ("job_status_not_started", JobStatusNotStarted)
    , ("job_status_running", JobStatusRunning)
    , ("job_status_failed", JobStatusFailed)
    , ("job_status_timed_out", JobStatusTimedOut)
    , ("job_status_succeeded", JobStatusSucceeded)
    , ("job_status_retry", JobStatusRetry)
    ]

textToEnumJobStatus :: Text -> Maybe JobStatus
textToEnumJobStatus t = HashMap.lookup t textToEnumJobStatusMap

-- | DefaultParamEncoder for hasql queries using JobStatus in filterWhere
instance DefaultParamEncoder JobStatus where
    defaultParam = Encoders.nonNullable (Encoders.enum (Just "public") "job_status" inputValue)

-- | DefaultParamEncoder for lists of JobStatus, needed for filterWhereIn/filterWhereNotIn
instance DefaultParamEncoder [JobStatus] where
    defaultParam = Encoders.nonNullable $ Encoders.foldableArray $ Encoders.nonNullable (Encoders.enum (Just "public") "job_status" inputValue)
