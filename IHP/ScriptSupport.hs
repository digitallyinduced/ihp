{-|
Module: IHP.ScriptSupport
Description: Run scripts inside the framework context, but outside of the usual web request response lifecycle
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.ScriptSupport (runScript, Script) where

import IHP.Prelude
import qualified IHP.FrameworkConfig as Config
import IHP.ModelSupport
import qualified Database.PostgreSQL.Simple as PG

-- | A script is just an IO action which requires a database connection
type Script = (?modelContext :: ModelContext) => IO ()

-- | Initializes IHP and then runs the script inside the framework context
runScript :: Script -> IO ()
runScript taskMain = do
    modelContext <- createModelContext    
    let ?modelContext = modelContext
    taskMain

createModelContext = do
    databaseUrl <- Config.appDatabaseUrl
    conn <- PG.connectPostgreSQL databaseUrl 
    pure (ModelContext conn)
    