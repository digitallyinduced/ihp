module TurboHaskell.IDE.Types where

import ClassyPrelude
import System.Process.Internals
import qualified System.Process as Process
import qualified GHC.IO.Handle as Handle
import qualified System.FSNotify as FS
import qualified Network.WebSockets as Websocket
import qualified Data.ByteString.Char8 as ByteString

data ManagedProcess = ManagedProcess
    { inputHandle :: !Handle
    , outputHandle :: !Handle
    , errorHandle :: !Handle
    , processHandle :: !ProcessHandle
    } deriving (Show)

--data State = State
--    { postgres : :ManagedProcess
--    , statusServer :: IORef (Async ())
--    , applicationGHCI :: ManagedProcess
--    , fileWatcher :: Async ()
--    , liveReloadNotificationServer :: Async ()
--    }
    
type FileEventHandler = FS.Event -> IO ()

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
    ByteString.hPutStrLn inputHandle command
    Handle.hFlush inputHandle

data OutputLine = StandardOutput ByteString | ErrorOutput ByteString deriving (Show, Eq)

data Action = 
    UpdatePostgresState PostgresState
    | UpdateAppGHCIState AppGHCIState
    | AppModulesLoaded
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
    | PauseApp
    deriving (Show)

data PostgresState
    = PostgresNotStarted
    | StartingPostgres
    | PostgresStarted { process :: ManagedProcess, standardOutput :: IORef ByteString, errorOutput :: IORef ByteString }
    deriving (Show)

data AppGHCIState
    = AppGHCINotStarted
    | AppGHCILoading { process :: ManagedProcess }
    | AppGHCIModulesLoaded { process :: ManagedProcess }
    | RunningAppGHCI { process :: ManagedProcess }
    deriving (Show)

data LiveReloadNotificationServerState
    = LiveReloadNotificationServerNotStarted
    | LiveReloadNotificationServerStarted { server :: Async (), clients :: IORef [Websocket.Connection] }
    deriving (Show)

data FileWatcherState
    = FileWatcherNotStarted
    | FileWatcherStarted { thread :: Async () }
    deriving (Show)

data StatusServerState
    = StatusServerNotStarted
    | StatusServerStarted
        { serverRef :: IORef (Async ())
        , clients :: IORef [Websocket.Connection]
        , standardOutput :: IORef ByteString
        , errorOutput :: IORef ByteString
        }
    deriving (Show)

data CodeGenerationState
    = CodeGenerationNotStarted
    | CodeGenerationReady { process :: ManagedProcess, standardOutput :: IORef ByteString, errorOutput :: IORef ByteString }
    | CodeGenerationRunning { process :: ManagedProcess, standardOutput :: IORef ByteString, errorOutput :: IORef ByteString }
    | CodeGenerationFailed { process :: ManagedProcess, standardOutput :: IORef ByteString, errorOutput :: IORef ByteString }
    deriving (Show)

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
    } deriving (Show)

emptyAppState :: AppState 
emptyAppState = AppState
    { postgresState = PostgresNotStarted
    , appGHCIState = AppGHCINotStarted
    , statusServerState = StatusServerNotStarted
    , liveReloadNotificationServerState = LiveReloadNotificationServerNotStarted
    , fileWatcherState = FileWatcherNotStarted
    , codeGenerationState = CodeGenerationNotStarted
    }

data Context = Context { actionVar :: MVar Action }

dispatch :: (?context :: Context) => Action -> IO ()
dispatch = let Context { .. } = ?context in putMVar actionVar