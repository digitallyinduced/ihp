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

procDirenvAware :: (?context :: Context) => FilePath -> [String] -> Process.CreateProcess
procDirenvAware command args =
    if ?context.wrapWithDirenv
        then Process.proc "direnv" (["exec", ".", command] <> args)
        else Process.proc command args

sendGhciCommand :: (?context :: Context) => Handle -> ByteString -> IO ()
sendGhciCommand inputHandle command = do
    when (isDebugMode ?context) (Log.debug ("GHCI: " <> cs command :: Text))
    ByteString.hPutStrLn inputHandle command
    Handle.hFlush inputHandle

sendGhciCommands :: (?context :: Context) => Handle -> [ByteString] -> IO ()
sendGhciCommands handle commands = forM_ commands (sendGhciCommand handle)

data OutputLine = StandardOutput !ByteString | ErrorOutput !ByteString deriving (Show, Eq)


data Context = Context
    { portConfig :: !PortConfig
    , isDebugMode :: !Bool
    , logger :: !Log.Logger
    , ghciInChan :: !(Queue.InChan OutputLine) -- ^ Output of the app ghci is written here
    , ghciOutChan :: !(Queue.OutChan OutputLine) -- ^ Output of the app ghci is consumed here
    , liveReloadClients :: !(IORef (Map UUID Websocket.Connection))
    , wrapWithDirenv :: !Bool
    , lastSchemaCompilerError :: !(IORef (Maybe SomeException))
    }
