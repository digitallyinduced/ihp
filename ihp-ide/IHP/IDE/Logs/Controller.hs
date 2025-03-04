module IHP.IDE.Logs.Controller where

import IHP.ControllerPrelude
import IHP.IDE.ToolServer.Helper.Controller
import IHP.IDE.ToolServer.Types
import IHP.IDE.Logs.View.Logs
import qualified IHP.IDE.Types as DevServer
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Builder as ByteString
import qualified Control.Concurrent.MVar as MVar

instance Controller LogsController where
    action AppLogsAction = do
        currentDevServerState <- readDevServerState
        let statusServerState = currentDevServerState.statusServerState

        (standardOutput, errorOutput) <- case statusServerState of
                DevServer.StatusServerNotStarted -> pure ("", "")
                DevServer.StatusServerStarted { standardOutput, errorOutput } -> do
                    std <- cs . ByteString.unlines . reverse <$> readIORef standardOutput
                    err <- cs . ByteString.unlines . reverse <$> readIORef errorOutput
                    pure (std, err)
                DevServer.StatusServerPaused { standardOutput, errorOutput } -> do
                    std <- cs . ByteString.unlines . reverse <$> readIORef standardOutput
                    err <- cs . ByteString.unlines . reverse <$> readIORef errorOutput
                    pure (std, err)

        render LogsView { .. }

    action PostgresLogsAction = do
        currentDevServerState <- readDevServerState

        standardOutput <- cs . ByteString.toLazyByteString <$> MVar.readMVar currentDevServerState.postgresStandardOutput
        errorOutput <- cs . ByteString.toLazyByteString <$> MVar.readMVar currentDevServerState.postgresErrorOutput

        render LogsView { .. }

    action OpenEditorAction = do
        let path = param @Text "path"
        let line = paramOrDefault @Int 0 "line"
        let col = paramOrDefault @Int 0 "col"
        openEditor path line col

        renderPlain ""

readDevServerState :: (?context :: ControllerContext) => IO DevServer.AppState
readDevServerState = ((.appStateRef) <$> theDevServerContext) >>= readIORef