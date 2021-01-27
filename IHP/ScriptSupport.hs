{-|
Module: IHP.ScriptSupport
Description: Run scripts inside the framework context, but outside of the usual web request response lifecycle
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.ScriptSupport (runScript, Script, module IHP.FrameworkConfig) where

import IHP.Prelude
import IHP.FrameworkConfig
import qualified IHP.Environment as Env
import IHP.ModelSupport
import IHP.ApplicationContext
import qualified Database.PostgreSQL.Simple as PG

-- | A script is just an IO action which requires a database connection and framework config
type Script = (?modelContext :: ModelContext, ?context :: FrameworkConfig) => IO ()

-- | Initializes IHP and then runs the script inside the framework context
runScript :: ConfigBuilder -> Script -> IO ()
runScript configBuilder taskMain = do
    frameworkConfig@FrameworkConfig { environment, dbPoolIdleTime, dbPoolMaxConnections, databaseUrl, logger } <- buildFrameworkConfig configBuilder
    modelContext <- createModelContext dbPoolIdleTime dbPoolMaxConnections databaseUrl logger

    let ?modelContext = modelContext
    let ?context = frameworkConfig
    taskMain
{-# INLINE runScript #-}