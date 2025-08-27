module IHP.Environment where
import IHP.Prelude

-- | The 'Environment' type is used to switch between development and production configurations.
--
-- When running 'devenv up', this will be set to 'Development', while 'deploy-to-nixos' will set it to 'Production'.
-- You can also manually set it with 'option Development' or 'option Production' in your Config.hs, or via the 'IHP_ENV' environment variable.
--
-- You can check the current environment using 'isDevelopment', 'isProduction', or 'isEnvironment' if you want to change behaviour based on the environment.
-- IHP by default implements the following differences:
--
-- - Static file caching: In 'Development', browser cache is disabled (max-age=0). In 'Production', max-age is forever, with asset hashes for invalidation. See 'initStaticApp'.
-- - Logger: In 'Development', logging uses the default format. In 'Production', it uses an Apache-style logger and higher log level. See 'ihpDefaultConfig'.
-- - Background workers: In 'Development', a development job worker server is started automatically. In 'Production', you need to manually start a separate RunJobs process. See the [chapter on Jobs in the Guide](https://ihp.digitallyinduced.com/Guide/jobs.html#development-vs-production) for details.
-- - Database connections: The pool idle time is by default shorter in 'Development'. See 'ihpDefaultConfig'.
-- - Error pages: In 'Development', error pages may contain backtraces and details about the code, with links to the IDE. In 'Production', error pages do not contain implementation information.
data Environment = Development | Production deriving (Eq, Show)
