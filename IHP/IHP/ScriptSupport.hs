{-|
Module: IHP.ScriptSupport
Description: Run scripts inside the framework context, but outside of the usual web request response lifecycle
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.ScriptSupport (runScript, Script, module IHP.FrameworkConfig) where

import IHP.Prelude
import IHP.FrameworkConfig
import IHP.ModelSupport
import Control.Exception (finally)
import IHP.Log (Logger(cleanup))
import Main.Utf8 (withUtf8)

-- | A script is just an IO action which requires a database connection and framework config
type Script = (?modelContext :: ModelContext, ?context :: FrameworkConfig) => IO ()

-- | Initializes IHP and then runs the script inside the framework context
runScript :: ConfigBuilder -> Script -> IO ()
runScript configBuilder taskMain = withUtf8 do
    frameworkConfig@FrameworkConfig { environment, dbPoolIdleTime, dbPoolMaxConnections, databaseUrl, logger } <- buildFrameworkConfig configBuilder
    modelContext <- createModelContext dbPoolIdleTime dbPoolMaxConnections databaseUrl logger

    let ?modelContext = modelContext
    let ?context = frameworkConfig
    taskMain `finally` cleanup logger
{-# INLINABLE runScript #-}