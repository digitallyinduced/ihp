module TurboHaskell.IDE.Types where

import ClassyPrelude
import System.Process.Internals
import qualified System.Process as Process
import qualified GHC.IO.Handle as Handle
import qualified System.FSNotify as FS

data ManagedProcess = ManagedProcess
    { inputHandle :: !Handle
    , outputHandle :: !Handle
    , errorHandle :: !Handle
    , processHandle :: !ProcessHandle
    }

data State = State
    { postgres :: ManagedProcess
    , statusServer :: IORef (Async ())
    , applicationGHCI :: ManagedProcess
    , fileWatcher :: Async ()
    , liveReloadNotificationServer :: Async ()
    }
    
type FileEventHandler = FS.Event -> IO ()

createManagedProcess :: CreateProcess -> IO ManagedProcess
createManagedProcess config = do
    process <- Process.createProcess config
    case process of
        (Just inputHandle, Just outputHandle, Just errorHandle, processHandle) -> pure ManagedProcess { .. }
        _ -> error "createManagedProcess: Some pipes could not be created"

cleanupManagedProcess :: ManagedProcess -> IO ()
cleanupManagedProcess (ManagedProcess { .. }) = Process.cleanupProcess (Just inputHandle, Just outputHandle, Just errorHandle, processHandle)

sendGhciCommand :: ManagedProcess -> String -> IO ()
sendGhciCommand ManagedProcess { inputHandle } command = do
    Handle.hPutStr inputHandle (command <> "\n")
    Handle.hFlush inputHandle