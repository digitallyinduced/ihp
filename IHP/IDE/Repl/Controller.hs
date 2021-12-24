module IHP.IDE.Repl.Controller where

import IHP.ControllerPrelude
import Control.Concurrent ( threadDelay, newEmptyMVar, takeMVar, putMVar )
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Helper.Controller
import qualified IHP.IDE.Types as DevServer
import qualified Data.Text as T
import qualified Data.ByteString.Builder
import Data.Functor ((<&>))
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import qualified IHP.Log as Log

import IHP.IDE.Repl.View

instance Controller ReplController where
    action ReplAction = do
        render ReplView { replLines = [], replCommands = [] }

instance WSApp ReplWSApp where
    initialState = InitReplSocket

    run = forever do
        state <- getState
        case state of
            InitReplSocket -> do
                setState ListenReplSocket
                putStrLn "Now listening..."
                pure ()
            ListenReplSocket -> do
                val <- receiveData @ByteString
                DevServer.ReplStarted { inputLine } <- get #replState <$> readDevServerState
                putMVar inputLine val
                pure ()

instance WSApp ReplOutputApp where
    initialState = ReplOutputInit
    run = do
        newOutputAvailable <- newEmptyMVar @()

        forever do
            state <- getState
            case state of
                ReplOutputInit -> do
                    -- race conditions!!!
                    DevServer.ReplStarted { outputLines, clients } <- get #replState <$> readDevServerState
                    currentOutput <- ByteString.unlines <$> readIORef outputLines
                    sendTextData currentOutput

                    atomicModifyIORef' clients (\clients -> (newOutputAvailable : clients, ()))
                    setState ReplOutputListen
                ReplOutputListen -> do
                    _ <- takeMVar newOutputAvailable
                    DevServer.ReplStarted { outputLines } <- get #replState <$> readDevServerState
                    currentOutput <- ByteString.unlines <$> readIORef outputLines
                    sendTextData currentOutput


readDevServerState :: (?context :: ControllerContext) => IO DevServer.AppState
readDevServerState = (get #appStateRef <$> theDevServerContext) >>= readIORef
