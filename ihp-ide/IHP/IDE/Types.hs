module IHP.IDE.Types where

import ClassyPrelude
import qualified System.Process as Process
import qualified GHC.IO.Handle as Handle
import qualified Network.WebSockets as Websocket
import qualified Data.ByteString.Char8 as ByteString
import IHP.IDE.PortConfig
import Data.String.Conversions (cs)
import Data.UUID
import System.Log.FastLogger (FastLogger, toLogStr)
import qualified Control.Concurrent.Chan.Unagi as Queue
import qualified Network.Socket as Socket
import System.OsPath (OsPath, decodeUtf)

procDirenvAware :: (?context :: Context) => OsPath -> [String] -> IO Process.CreateProcess
procDirenvAware command args = do
    commandStr <- decodeUtf command
    pure if ?context.wrapWithDirenv
        then Process.proc "direnv" (["exec", ".", commandStr] <> args)
        else Process.proc commandStr args

sendGhciCommand :: (?context :: Context) => Handle -> ByteString -> IO ()
sendGhciCommand inputHandle command = do
    when (isDebugMode ?context) (?context.logger (toLogStr ("GHCI: " <> cs command :: Text)))
    ByteString.hPutStrLn inputHandle command
    Handle.hFlush inputHandle

sendGhciCommands :: (?context :: Context) => Handle -> [ByteString] -> IO ()
sendGhciCommands handle commands = forM_ commands (sendGhciCommand handle)

data OutputLine = StandardOutput !ByteString | ErrorOutput !ByteString deriving (Show, Eq)

-- | Extract the runtime crash message from the accumulated app/GHCi output.
--
-- When running the app, the dev server wraps its @main@ in a handler that, on an
-- uncaught exception, prints the exception text between two marker lines:
--
-- > [[IHP_APP_CRASHED_BEGIN]]
-- > <exception text, possibly spanning multiple lines>
-- > [[IHP_APP_CRASHED]]
--
-- This returns just the exception lines (without the markers), so a startup crash
-- (e.g. a missing env var) can be surfaced as the prominent error on the status
-- page instead of being buried at the bottom of the build log. Returns @[]@ when
-- no crash message is present.
extractCrashMessage :: ByteString -> [ByteString]
extractCrashMessage accumulatedOutput =
    let
        allLines = ByteString.lines accumulatedOutput
        afterBeginMarker = drop 1 (dropWhile (not . isInfixOf "[[IHP_APP_CRASHED_BEGIN]]") allLines)
    in
        takeWhile (not . isInfixOf "[[IHP_APP_CRASHED]]") afterBeginMarker


data Context = Context
    { portConfig :: !PortConfig
    , isDebugMode :: !Bool
    , logger :: !FastLogger
    , ghciInChan :: !(Queue.InChan OutputLine) -- ^ Output of the app ghci is written here
    , ghciOutChan :: !(Queue.OutChan OutputLine) -- ^ Output of the app ghci is consumed here
    , liveReloadClients :: !(IORef (Map UUID Websocket.Connection))
    , wrapWithDirenv :: !Bool
    , lastSchemaCompilerError :: !(IORef (Maybe SomeException))
    , appSocket :: !Socket.Socket -- ^ Shared socket for seamless app restarts
    }
