module Main where

import IHP.Prelude
import IHP.SchemaMigration
import IHP.ModelSupport
import IHP.FrameworkConfig
import IHP.Log.Types

main :: IO ()
main = do
    frameworkConfig <- buildFrameworkConfig (pure ())

    -- We need a debug logger to print out all sql queries during the migration.
    -- The production env logger could be set to a different log level, therefore
    -- we don't use the logger in 'frameworkConfig'
    --
    logger <- defaultLogger

    modelContext <- createModelContext
        (get #dbPoolIdleTime frameworkConfig)
        (get #dbPoolMaxConnections frameworkConfig)
        (get #databaseUrl frameworkConfig)
        logger

    let ?modelContext = modelContext

    minimumRevision <- envOrNothing "MINIMUM_REVISION"
    migrate MigrateOptions { minimumRevision }

    logger |> cleanup