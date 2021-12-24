module IHP.IDE.Types where

import ClassyPrelude
import System.Process.Internals
import Control.Concurrent (MVar(..))
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
    | ReceiveAppOutput { line :: !OutputLine }
    | AssetChanged
    | HaskellFileChanged
    | SchemaChanged
    | UpdateStatusServerState !StatusServerState
    | UpdateReplState !ReplState
    | UpdateLiveReloadNotificationServerState !LiveReloadNotificationServerState
    | UpdateFileWatcherState !FileWatcherState
    | UpdateToolServerState !ToolServerState
    | PauseApp
    deriving (Show)

data PostgresState
    = PostgresNotStarted
    | StartingPostgres
    | PostgresStarted { process :: !ManagedProcess, standardOutput :: !(IORef ByteString.Builder), errorOutput :: !(IORef ByteString.Builder) }

instance Show PostgresState where
    show PostgresNotStarted = "NotStarted"
    show StartingPostgres = "Starting"
    show PostgresStarted { } = "Started"

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

data LiveReloadNotificationServerState
    = LiveReloadNotificationServerState { clients :: !(IORef (Map UUID Websocket.Connection)) }

instance Show LiveReloadNotificationServerState where
    show LiveReloadNotificationServerState { } = "LiveReloadNotificationServerState"

data FileWatcherState
    = FileWatcherNotStarted
    | FileWatcherStarted { thread :: !(Async ()) }

instance Show FileWatcherState where
    show FileWatcherNotStarted = "NotStarted"
    show FileWatcherStarted { } = "Started"

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

data ToolServerState
    = ToolServerNotStarted
    | ToolServerStarted { thread :: !(Async ()) }

instance Show ToolServerState where
    show ToolServerNotStarted = "NotStarted"
    show ToolServerStarted {} = "Started"

data ReplState
    = ReplNotStarted
    | ReplStarted
        { process :: !ManagedProcess
        , inputLine :: !(MVar ByteString)
        , outputLines :: !(IORef [ByteString])
        , clients :: !(IORef [MVar ()])
        , stdoutListener :: Async ()
        , stderrListener :: Async ()
        , inputListener :: Async ()
        }

instance Show ReplState where
    show ReplNotStarted = "ReplNotStarted"
    show ReplStarted {} = "ReplStarted"


instance Show (IORef x) where show _ = "(..)"
instance Show ProcessHandle where show _ = "(..)"
instance Show (Async ()) where show _ = "(..)"

data AppState = AppState
    { postgresState :: !PostgresState
    , appGHCIState :: !AppGHCIState
    , replState :: !ReplState
    , statusServerState :: !StatusServerState
    , liveReloadNotificationServerState :: !LiveReloadNotificationServerState
    , fileWatcherState :: !FileWatcherState
    , toolServerState :: !ToolServerState
    , databaseNeedsMigration :: !(IORef Bool)
    } deriving (Show)

emptyAppState :: IO AppState
emptyAppState = do
    clients <- newIORef mempty
    databaseNeedsMigration <- newIORef False
    pure AppState
        { postgresState = PostgresNotStarted
        , appGHCIState = AppGHCINotStarted
        , replState = ReplNotStarted
        , statusServerState = StatusServerNotStarted
        , liveReloadNotificationServerState = LiveReloadNotificationServerState { clients }
        , fileWatcherState = FileWatcherNotStarted
        , toolServerState = ToolServerNotStarted
        , databaseNeedsMigration
        }

data Context = Context
    { actionVar :: !(MVar Action)
    , portConfig :: !PortConfig
    , appStateRef :: !(IORef AppState)
    , isDebugMode :: !Bool
    , logger :: !Log.Logger
    }

dispatch :: (?context :: Context) => Action -> IO ()
dispatch = let Context { .. } = ?context in putMVar actionVar

instance Log.LoggingProvider Context where
    getLogger Context { logger } = logger
