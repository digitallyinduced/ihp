{-|
Module: IHP.ScriptSupport
Description: Run scripts inside the framework context, but outside of the usual web request response lifecycle
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.ScriptSupport (runScript, Script) where

import IHP.Prelude
import IHP.FrameworkConfig
import qualified IHP.Environment as Env
import IHP.ModelSupport
import qualified Database.PostgreSQL.Simple as PG

-- | A script is just an IO action which requires a database connection and framework config
type Script = (?modelContext :: ModelContext, ?frameworkConfig :: FrameworkConfig) => IO ()

-- | Initializes IHP and then runs the script inside the framework context
runScript :: ConfigBuilder -> Script -> IO ()
runScript configBuilder taskMain = do
    frameworkConfig@FrameworkConfig { environment, dbPoolIdleTime, dbPoolMaxConnections, databaseUrl } <- buildFrameworkConfig configBuilder
    modelContext <- (\modelContext -> modelContext { queryDebuggingEnabled = Env.isDevelopment environment }) <$> createModelContext dbPoolIdleTime dbPoolMaxConnections databaseUrl
    let ?modelContext = modelContext
    let ?frameworkConfig = frameworkConfig
    taskMain
