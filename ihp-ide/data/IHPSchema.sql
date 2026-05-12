-- Provides all the default settings for a IHP database in development mode
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

-- Used by IHP.Job
CREATE TYPE JOB_STATUS AS ENUM ('job_status_not_started', 'job_status_running', 'job_status_failed', 'job_status_timed_out', 'job_status_succeeded', 'job_status_retry');

-- Used by IHP.DataSync
-- Returns NULL when the setting is unset or empty, so RLS policies
-- like @user_id = ihp_user_id()@ evaluate to NULL (filtering all rows)
-- instead of raising @invalid input syntax for type uuid: ""@.
-- CREATE OR REPLACE so older databases get the robust version on next migrate.
CREATE OR REPLACE FUNCTION ihp_user_id() RETURNS UUID AS $$
    SELECT NULLIF(current_setting('rls.ihp_user_id', true), '')::uuid;
$$ LANGUAGE SQL;
