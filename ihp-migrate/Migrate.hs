module Main where

import IHP.Prelude
import IHP.SchemaMigration
import IHP.ModelSupport
import IHP.FrameworkConfig
import IHP.Log.Types
import Main.Utf8 (withUtf8)
import qualified IHP.EnvVar as EnvVar

main :: IO ()
main = withUtf8 do
    frameworkConfig <- buildFrameworkConfig (pure ())

    -- We need a debug logger to print out all sql queries during the migration.
    -- The production env logger could be set to a different log level, therefore
    -- we don't use the logger in 'frameworkConfig'
    --
    logger <- defaultLogger

    modelContext <- createModelContext
        frameworkConfig.dbPoolIdleTime
        frameworkConfig.dbPoolMaxConnections
        frameworkConfig.databaseUrl
        logger

    let ?modelContext = modelContext

    minimumRevision <- EnvVar.envOrNothing "MINIMUM_REVISION"
    migrate MigrateOptions { minimumRevision }

    logger |> cleanup