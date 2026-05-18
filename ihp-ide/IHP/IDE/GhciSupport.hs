{-|
GHCi-process plumbing shared by the dev mode binaries
('IHP.IDE.DevServer' and 'IHP.IDE.DevWorker').

This module is deliberately context-free — every function takes its inputs
explicitly so it can be used both by 'DevServer' (which has a rich 'Context'
of dev-server state) and 'DevWorker' (which has almost none).
-}
module IHP.IDE.GhciSupport
( -- * Spawning GHCi
  ghciArguments
, procGhci
, withGhci
  -- * Talking to GHCi
, sendGhciCommand
, sendGhciCommands
  -- * Reading GHCi output
, readHandleLines
  -- * Stopping GHCi
, withSigTermHandler
, stopProcessHandle
) where

import ClassyPrelude
import qualified Control.Concurrent as Concurrent
import qualified Control.Exception.Safe as Exception
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Builder as ByteString
import qualified GHC.IO.Handle as Handle
import qualified System.Exit as Exit
import qualified System.Posix.Signals as Signals
import qualified System.Process as Process

-- | Default GHCi argv used by every IHP dev-mode GHCi child.
ghciArguments :: [String]
ghciArguments =
    [ "-threaded"
    , "-fomit-interface-pragmas"
    , "-j"
    , "-O0"
    , "-package-env -" -- Don't load global package environments — they conflict with our pinned set
    , "-ignore-dot-ghci" -- Skip the global ~/.ghc/ghci.conf which sometimes sets `+c +s`
    , "-ghci-script", ".ghci" -- Manually point at the project's .ghci since we just disabled defaults
    , "+RTS", "-A64m", "-n4m", "-H256m", "--nonmoving-gc", "-Iw60", "-N4"
    ]

-- | Build a 'Process.CreateProcess' that launches GHCi, optionally wrapped in
-- @direnv exec .@ so dev-shell environment variables are inherited.
procGhci :: Bool -> [String] -> Process.CreateProcess
procGhci wrapWithDirenv args =
    if wrapWithDirenv
        then Process.proc "direnv" (["exec", ".", "ghci"] <> args)
        else Process.proc "ghci" args

-- | Spawn GHCi, wire up its stdio pipes, install a SIGTERM handler that
-- gracefully tears down the GHCi process group, and run the callback with
-- the resulting handles. The process is in its own process group so that
-- 'stopProcessHandle' can target the whole tree.
withGhci
    :: Bool                -- ^ wrap with @direnv exec .@?
    -> Concurrent.ThreadId -- ^ thread to interrupt with 'Exit.ExitSuccess' on SIGTERM
    -> (Handle -> Handle -> Handle -> Process.ProcessHandle -> IO a)
    -> IO a
withGhci wrapWithDirenv mainThreadId callback = do
    let baseParams = procGhci wrapWithDirenv ghciArguments
    let params = baseParams
            { Process.std_in = Process.CreatePipe
            , Process.std_out = Process.CreatePipe
            , Process.std_err = Process.CreatePipe
            , Process.create_group = True
            }

    Process.withCreateProcess params \(Just input) (Just output) (Just err) processHandle -> do
        let sigTermHandler = do
                stopProcessHandle processHandle
                Concurrent.throwTo mainThreadId Exit.ExitSuccess
        withSigTermHandler sigTermHandler (callback input output err processHandle)

-- | Write a single command to a GHCi process's stdin (no logging).
sendGhciCommand :: Handle -> ByteString -> IO ()
sendGhciCommand inputHandle command = do
    ByteString.hPutStrLn inputHandle command
    Handle.hFlush inputHandle

sendGhciCommands :: Handle -> [ByteString] -> IO ()
sendGhciCommands handle commands = forM_ commands (sendGhciCommand handle)

-- | Read lines from a handle, accumulating output and classifying each line.
-- Races against @stopVar@ being filled — when the MVar is readable, reading stops.
readHandleLines
    :: MVar a                   -- ^ Race against this (stop when filled)
    -> MVar ByteString.Builder  -- ^ Output accumulator
    -> Handle                   -- ^ Handle to read from
    -> (ByteString -> IO ())    -- ^ Log callback, called for every line
    -> (ByteString -> IO ())    -- ^ Classifier/action, called for every line
    -> IO ()
readHandleLines stopVar outputVar handle logLine onMatch = race_ (readMVar stopVar) $ forever do
    line <- ByteString.hGetLine handle
    modifyMVar_ outputVar (\builder -> pure (builder <> "\n" <> ByteString.byteString line))
    logLine line
    onMatch line

-- | Install a SIGTERM handler for the duration of the callback, restoring
-- the previous handler on exit.
withSigTermHandler :: IO () -> IO a -> IO a
withSigTermHandler sigTermHandler callback = Exception.bracket
    (Signals.installHandler Signals.sigTERM (Signals.Catch sigTermHandler) Nothing)
    (\previous -> void (Signals.installHandler Signals.sigTERM previous Nothing))
    (\_ -> callback)

-- | Stop a GHCi process by signalling its whole process group with SIGTERM,
-- waiting up to 1 second, then escalating to SIGKILL.
stopProcessHandle :: Process.ProcessHandle -> IO ()
stopProcessHandle processHandle = do
    maybePid <- Process.getPid processHandle `Exception.catchAny` \_ -> pure Nothing
    let signalGroup signal = case maybePid of
            Just pid ->
                Signals.signalProcessGroup signal pid
                    `Exception.catchAny` \_ ->
                        Signals.signalProcess signal pid
                            `Exception.catchAny` \_ -> pure ()
            Nothing -> Process.terminateProcess processHandle `Exception.catchAny` \_ -> pure ()

    signalGroup Signals.sigTERM
    exited <- isJust <$> timeout (1 * 1000000) (Process.waitForProcess processHandle `Exception.catchAny` \_ -> pure Exit.ExitSuccess)
    unless exited do
        signalGroup Signals.sigKILL
        void (timeout (1 * 1000000) (Process.waitForProcess processHandle `Exception.catchAny` \_ -> pure Exit.ExitSuccess))
