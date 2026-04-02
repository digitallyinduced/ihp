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
