module TurboHaskell.IDE.Types where

import ClassyPrelude
import System.Process.Internals
import qualified System.Process as Process
import qualified GHC.IO.Handle as Handle
import qualified System.FSNotify as FS
import qualified Network.WebSockets as Websocket
import qualified Data.ByteString.Char8 as ByteString
import TurboHaskell.IDE.PortConfig
import Data.String.Conversions (cs)

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

sendGhciCommand :: ManagedProcess -> ByteString -> IO ()
sendGhciCommand ManagedProcess { inputHandle } command = do
    putStrLn ("GHCI: " <> cs command)
    ByteString.hPutStrLn inputHandle command
    Handle.hFlush inputHandle

data OutputLine = StandardOutput ByteString | ErrorOutput ByteString deriving (Show, Eq)

data Action = 
    UpdatePostgresState PostgresState
    | UpdateAppGHCIState AppGHCIState
    | AppModulesLoaded { success :: Bool }
    | AppStarted
    | ReceiveAppOutput { line :: OutputLine }
    | ReceiveCodeGenerationOutput { line :: OutputLine }
    | AssetChanged
    | HaskellFileChanged
    | SchemaChanged
    | UpdateStatusServerState StatusServerState
    | UpdateLiveReloadNotificationServerState LiveReloadNotificationServerState
    | UpdateFileWatcherState FileWatcherState
    | UpdateCodeGenerationState CodeGenerationState
    | UpdateToolServerState ToolServerState
    | PauseApp
    deriving (Show)

data PostgresState
    = PostgresNotStarted
    | StartingPostgres
    | PostgresStarted { process :: ManagedProcess, standardOutput :: IORef ByteString, errorOutput :: IORef ByteString }

instance Show PostgresState where
    show PostgresNotStarted = "NotStarted"
    show StartingPostgres = "Starting"
    show PostgresStarted { } = "Started"

data AppGHCIState
    = AppGHCINotStarted
    | AppGHCILoading { process :: ManagedProcess, needsErrorRecovery :: IORef Bool, isFirstStart :: IORef Bool }
    | AppGHCIModulesLoaded { process :: ManagedProcess, needsErrorRecovery :: IORef Bool, isFirstStart :: IORef Bool }
    | RunningAppGHCI { process :: ManagedProcess, needsErrorRecovery :: IORef Bool, isFirstStart :: IORef Bool }

instance Show AppGHCIState where
    show AppGHCINotStarted = "NotStarted"
    show AppGHCILoading { } = "Loading"
    show AppGHCIModulesLoaded { } = "Loaded"
    show RunningAppGHCI { } = "Running"

data LiveReloadNotificationServerState
    = LiveReloadNotificationServerNotStarted
    | LiveReloadNotificationServerStarted { server :: Async (), clients :: IORef [Websocket.Connection] }

instance Show LiveReloadNotificationServerState where
    show LiveReloadNotificationServerNotStarted = "NotStarted"
    show LiveReloadNotificationServerStarted { } = "Started"

data FileWatcherState
    = FileWatcherNotStarted
    | FileWatcherStarted { thread :: Async () }

instance Show FileWatcherState where
    show FileWatcherNotStarted = "NotStarted"
    show FileWatcherStarted { } = "Started"

data StatusServerState
    = StatusServerNotStarted
    | StatusServerStarted
        { serverRef :: IORef (Async ())
        , clients :: IORef [Websocket.Connection]
        , standardOutput :: IORef ByteString
        , errorOutput :: IORef ByteString
        }
    | StatusServerPaused
        { serverRef :: IORef (Async ())
        , clients :: IORef [Websocket.Connection]
        , standardOutput :: IORef ByteString
        , errorOutput :: IORef ByteString
        }

instance Show StatusServerState where
    show StatusServerNotStarted = "NotStarted"
    show StatusServerStarted { } = "Started"
    show StatusServerPaused { } = "Paused"

data ToolServerState
    = ToolServerNotStarted
    | ToolServerStarted { thread :: Async () }

instance Show ToolServerState where
    show ToolServerNotStarted = "NotStarted"
    show ToolServerStarted {} = "Started"


data CodeGenerationState
    = CodeGenerationNotStarted
    | CodeGenerationReady { process :: ManagedProcess, standardOutput :: IORef ByteString, errorOutput :: IORef ByteString }
    | CodeGenerationRunning { process :: ManagedProcess, standardOutput :: IORef ByteString, errorOutput :: IORef ByteString }
    | CodeGenerationFailed { process :: ManagedProcess, standardOutput :: IORef ByteString, errorOutput :: IORef ByteString }

instance Show CodeGenerationState where
    show CodeGenerationNotStarted = "NotStarted"
    show CodeGenerationReady { } = "Ready"
    show CodeGenerationRunning { } = "Running"
    show CodeGenerationFailed { } = "Failed"

instance Show (IORef x) where show _ = "(..)"
instance Show ProcessHandle where show _ = "(..)"
instance Show (Async ()) where show _ = "(..)"

data AppState = AppState
    { postgresState :: PostgresState
    , appGHCIState :: AppGHCIState
    , statusServerState :: StatusServerState
    , liveReloadNotificationServerState :: LiveReloadNotificationServerState
    , fileWatcherState :: FileWatcherState
    , codeGenerationState :: CodeGenerationState
    , toolServerState :: ToolServerState
    } deriving (Show)

emptyAppState :: AppState 
emptyAppState = AppState
    { postgresState = PostgresNotStarted
    , appGHCIState = AppGHCINotStarted
    , statusServerState = StatusServerNotStarted
    , liveReloadNotificationServerState = LiveReloadNotificationServerNotStarted
    , fileWatcherState = FileWatcherNotStarted
    , codeGenerationState = CodeGenerationNotStarted
    , toolServerState = ToolServerNotStarted
    }

data Context = Context
    { actionVar :: MVar Action
    , portConfig :: PortConfig
    }

dispatch :: (?context :: Context) => Action -> IO ()
dispatch = let Context { .. } = ?context in putMVar actionVar


