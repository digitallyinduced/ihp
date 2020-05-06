module TurboHaskell.FrameworkConfig where

import ClassyPrelude
import qualified System.Environment as Environment
import System.Directory (getCurrentDirectory)
import TurboHaskell.Environment
import TurboHaskell.ControllerSupport
import TurboHaskell.RouterSupport
import System.IO.Unsafe (unsafePerformIO)
import Data.String.Conversions (cs)

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

instance Controller RootApplication where

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