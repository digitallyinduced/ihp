module TurboHaskell.TaskSupport (runTask, Task) where

import TurboHaskell.Prelude
import qualified TurboHaskell.FrameworkConfig as Config
import TurboHaskell.ModelSupport
import qualified Database.PostgreSQL.Simple as PG

type Task = (?modelContext :: ModelContext) => IO ()

runTask :: Task -> IO ()
runTask taskMain = do
    modelContext <- createModelContext    
    let ?modelContext = modelContext
    putStrLn "\n\nSTARTING TASK"
    taskMain
    putStrLn "TASK FINISHED\n\n"

createModelContext = do
    databaseUrl <- Config.appDatabaseUrl
    conn <- PG.connectPostgreSQL databaseUrl 
    pure (ModelContext conn)
    