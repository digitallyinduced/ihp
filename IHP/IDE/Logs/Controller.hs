module IHP.IDE.Logs.Controller where

import IHP.ControllerPrelude
import IHP.IDE.ToolServer.Helper.Controller
import IHP.IDE.ToolServer.Types
import IHP.IDE.Logs.View.Logs
import qualified IHP.IDE.Types as DevServer
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Builder as ByteString

instance Controller LogsController where
    action AppLogsAction = do
        currentDevServerState <- readDevServerState
        let statusServerState = currentDevServerState |> get #statusServerState

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
        let postgresState = currentDevServerState |> get #postgresState

        (standardOutput, errorOutput) <- case postgresState of
                DevServer.PostgresStarted { standardOutput, errorOutput } -> do
                    err <- cs . ByteString.toLazyByteString <$> readIORef errorOutput
                    std <- cs . ByteString.toLazyByteString <$> readIORef standardOutput
                    pure (std, err)
                _ -> pure ("", "")

        render LogsView { .. }

    action OpenEditorAction = do
        let path = param @Text "path"
        let line = paramOrDefault @Int 0 "line"
        let col = paramOrDefault @Int 0 "col"
        openEditor path line col

        renderPlain ""

readDevServerState :: (?context :: ControllerContext) => IO DevServer.AppState
readDevServerState = (get #appStateRef <$> theDevServerContext) >>= readIORef

theDevServerContext :: (?context :: ControllerContext) => IO DevServer.Context
theDevServerContext = get #devServerContext <$> (fromContext @ToolServerApplication)