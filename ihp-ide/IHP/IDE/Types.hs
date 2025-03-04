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

procDirenvAware :: (?context :: Context) => FilePath -> [String] -> Process.CreateProcess
procDirenvAware command args =
    if ?context.wrapWithDirenv
        then Process.proc "direnv" (["exec", ".", command] <> args)
        else Process.proc command args

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
    UpdateAppGHCIState AppGHCIState
    | AppModulesLoaded { success :: !Bool }
    | AppStarted
    | HaskellFileChanged
    | UpdateStatusServerState !StatusServerState
    | PauseApp
    deriving (Show)

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
    { appGHCIState :: !AppGHCIState
    , statusServerState :: !StatusServerState
    , databaseNeedsMigration :: !(IORef Bool)
    , lastSchemaCompilerError :: !(IORef (Maybe SomeException))
    , postgresStandardOutput :: (MVar ByteString.Builder)
    , postgresErrorOutput :: (MVar ByteString.Builder)
    }

emptyAppState :: IO AppState
emptyAppState = do
    databaseNeedsMigration <- newIORef False
    lastSchemaCompilerError <- newIORef Nothing
    postgresStandardOutput <- newEmptyMVar
    postgresErrorOutput <- newEmptyMVar
    pure AppState
        { appGHCIState = AppGHCINotStarted
        , statusServerState = StatusServerNotStarted
        , databaseNeedsMigration
        , lastSchemaCompilerError
        , postgresStandardOutput
        , postgresErrorOutput
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
    , wrapWithDirenv :: !Bool
    }

dispatch :: (?context :: Context) => Action -> IO ()
dispatch = let Context { .. } = ?context in putMVar actionVar