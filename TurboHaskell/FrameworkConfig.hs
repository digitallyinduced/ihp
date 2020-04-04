module TurboHaskell.FrameworkConfig where

import ClassyPrelude
import qualified System.Environment as Environment
import System.Directory (getCurrentDirectory)
import TurboHaskell.Environment
import TurboHaskell.ControllerSupport
import TurboHaskell.RouterSupport

class FrameworkConfig where
    baseUrl :: Text
    environment :: Environment


data RootApplication = RootApplication deriving (Eq, Show)

instance Controller RootApplication where

appDatabaseUrl :: IO ByteString
appDatabaseUrl = do
    currentDirectory <- getCurrentDirectory
    let defaultDatabaseUrl = "postgresql:///app?host=" <> cs currentDirectory <> "/build/db"
    (Environment.lookupEnv "DATABASE_URL") >>= (pure . maybe defaultDatabaseUrl cs )