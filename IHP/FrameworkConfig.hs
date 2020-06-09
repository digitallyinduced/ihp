module IHP.FrameworkConfig where

import ClassyPrelude
import qualified System.Environment as Environment
import System.Directory (getCurrentDirectory)
import IHP.Environment
import IHP.ControllerSupport
import System.IO.Unsafe (unsafePerformIO)
import Data.String.Conversions (cs)
import qualified System.Directory as Directory
import qualified Data.Text as Text
import qualified System.Process as Process

defaultPort :: Int
defaultPort = 8000

portRef :: IORef Int
portRef = unsafePerformIO (newIORef defaultPort)

class FrameworkConfig where
    appHostname :: Text
    environment :: Environment

    appPort :: Int
    appPort = unsafePerformIO (readIORef portRef)
            
    baseUrl :: Text
    baseUrl = let port = appPort in "http://" <> appHostname <> (if port /= 80 then ":" <> tshow port else "")

data RootApplication = RootApplication deriving (Eq, Show)

initAppPort :: IO Int
initAppPort = do
    portStr <- Environment.lookupEnv "PORT"
    case portStr of
        Just portStr -> do
            let port = fromMaybe (error "PORT: Invalid value") (readMay portStr)
            writeIORef portRef port
            pure port
        Nothing -> pure defaultPort

appDatabaseUrl :: IO ByteString
appDatabaseUrl = do
    currentDirectory <- getCurrentDirectory
    let defaultDatabaseUrl = "postgresql:///app?host=" <> cs currentDirectory <> "/build/db"
    (Environment.lookupEnv "DATABASE_URL") >>= (pure . maybe defaultDatabaseUrl cs )

-- | Finds the lib
--
-- The location depends on whether the framework is installed through nix
-- or checked out from git inside the current project directory.
--
-- When it's installed with nix, the lib dir is located at @lib/ihp@
-- while the dev server binary is located at @bin/RunDevServer@.
findLibDirectory :: IO Text
findLibDirectory = do
    frameworkMountedLocally <- Directory.doesDirectoryExist "IHP"
    if frameworkMountedLocally
        then pure "IHP/lib/IHP/"
        else do
            binDir <- cs <$> Process.readCreateProcess (Process.shell "dirname $(which RunDevServer)") ""
            pure (Text.strip binDir <> "/../lib/IHP/")