module IHP.IDE.Types where

import ClassyPrelude
import System.Process.Internals
import qualified System.Process as Process
import qualified GHC.IO.Handle as Handle
import qualified Network.WebSockets as Websocket
import qualified Data.ByteString.Char8 as ByteString
import IHP.IDE.PortConfig
import Data.String.Conversions (cs)
import Data.UUID
import qualified IHP.Log.Types as Log
import qualified IHP.Log as Log
import qualified Data.ByteString.Builder as ByteString
import qualified Control.Concurrent.Chan.Unagi as Queue

data ManagedProcess = ManagedProcess
    { inputHandle :: !Handle
    , outputHandle :: !Handle
    , errorHandle :: !Handle
    , processHandle :: !ProcessHandle
    } deriving (Show)

createManagedProcess :: CreateProcess -> IO ManagedProcess
createManagedProcess config = do
    process <- Process.createProcess config
    case process of
        (Just inputHandle, Just outputHandle, Just errorHandle, processHandle) -> pure ManagedProcess { .. }
        _ -> error "createManagedProcess: Some pipes could not be created"

cleanupManagedProcess :: ManagedProcess -> IO ()
cleanupManagedProcess (ManagedProcess { .. }) = Process.cleanupProcess (Just inputHandle, Just outputHandle, Just errorHandle, processHandle)

sendGhciCommand :: (?context :: Context) => ManagedProcess -> ByteString -> IO ()
sendGhciCommand ManagedProcess { inputHandle } command = do
    when (isDebugMode ?context) (Log.debug ("GHCI: " <> cs command :: Text))
    ByteString.hPutStrLn inputHandle command
    Handle.hFlush inputHandle

data OutputLine = StandardOutput !ByteString | ErrorOutput !ByteString deriving (Show, Eq)

data Action =
    UpdatePostgresState PostgresState
    | UpdateAppGHCIState AppGHCIState
    | AppModulesLoaded { success :: !Bool }
    | AppStarted
    | AssetChanged
    | HaskellFileChanged
    | SchemaChanged
    | UpdateStatusServerState !StatusServerState
    | PauseApp
    deriving (Show)

data PostgresState
    = PostgresNotStarted
    | StartingPostgres
    | PostgresReady
    | PostgresStarted { process :: !ManagedProcess, standardOutput :: !(IORef ByteString.Builder), errorOutput :: !(IORef ByteString.Builder) }

instance Show PostgresState where
    show PostgresNotStarted = "NotStarted"
    show StartingPostgres = "Starting"
    show PostgresStarted { } = "Started"
    show PostgresReady { } = "Ready"

data AppGHCIState
    = AppGHCINotStarted
    | AppGHCILoading { process :: !ManagedProcess }
    | AppGHCIModulesLoaded { process :: !ManagedProcess }
    | RunningAppGHCI { process :: !ManagedProcess }

instance Show AppGHCIState where
    show AppGHCINotStarted = "NotStarted"
    show AppGHCILoading { } = "Loading"
    show AppGHCIModulesLoaded { } = "Loaded"
    show RunningAppGHCI { } = "Running"

data StatusServerState
    = StatusServerNotStarted
    | StatusServerStarted
        { serverRef :: !(IORef (Async ()))
        , clients :: !(IORef [(Websocket.Connection, MVar ())])
        , standardOutput :: !(IORef [ByteString])
        , errorOutput :: !(IORef [ByteString])
        }
    | StatusServerPaused
        { serverRef :: !(IORef (Async ()))
        , clients :: !(IORef [(Websocket.Connection, MVar ())])
        , standardOutput :: !(IORef [ByteString])
        , errorOutput :: !(IORef [ByteString])
        }

instance Show StatusServerState where
    show StatusServerNotStarted = "NotStarted"
    show StatusServerStarted { } = "Started"
    show StatusServerPaused { } = "Paused"


instance Show (IORef x) where show _ = "(..)"
instance Show ProcessHandle where show _ = "(..)"
instance Show (Async ()) where show _ = "(..)"

data AppState = AppState
    { postgresState :: !PostgresState
    , appGHCIState :: !AppGHCIState
    , statusServerState :: !StatusServerState
    , databaseNeedsMigration :: !(IORef Bool)
    , lastSchemaCompilerError :: !(IORef (Maybe SomeException))
    } deriving (Show)

emptyAppState :: IO AppState
emptyAppState = do
    databaseNeedsMigration <- newIORef False
    lastSchemaCompilerError <- newIORef Nothing
    pure AppState
        { postgresState = PostgresNotStarted
        , appGHCIState = AppGHCINotStarted
        , statusServerState = StatusServerNotStarted
        , databaseNeedsMigration
        , lastSchemaCompilerError
        }

data Context = Context
    { actionVar :: !(MVar Action)
    , portConfig :: !PortConfig
    , appStateRef :: !(IORef AppState)
    , isDebugMode :: !Bool
    , logger :: !Log.Logger
    , ghciInChan :: !(Queue.InChan OutputLine) -- ^ Output of the app ghci is written here
    , ghciOutChan :: !(Queue.OutChan OutputLine) -- ^ Output of the app ghci is consumed here
    , liveReloadClients :: !(IORef (Map UUID Websocket.Connection))
    }

dispatch :: (?context :: Context) => Action -> IO ()
dispatch = let Context { .. } = ?context in putMVar actionVar