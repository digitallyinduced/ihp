module Main where

import IHP.Prelude
import IHP.SchemaMigration
import IHP.ModelSupport
import IHP.FrameworkConfig

main :: IO ()
main = do
    frameworkConfig <- buildFrameworkConfig (pure ())
    modelContext <- createModelContext
        (get #dbPoolIdleTime frameworkConfig)
        (get #dbPoolMaxConnections frameworkConfig)
        (get #databaseUrl frameworkConfig)
        (get #logger frameworkConfig)

    let ?modelContext = modelContext
    migrate