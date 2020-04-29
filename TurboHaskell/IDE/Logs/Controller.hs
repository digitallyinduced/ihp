module TurboHaskell.IDE.Logs.Controller where

import TurboHaskell.ControllerPrelude
import TurboHaskell.IDE.ToolServer.Types
import TurboHaskell.IDE.ToolServer.ViewContext
import TurboHaskell.IDE.Logs.View.Logs
import qualified TurboHaskell.IDE.Types as DevServer

instance Controller LogsController where
    action AppLogsAction = do
        currentDevServerState <- readDevServerState
        let statusServerState = currentDevServerState |> get #statusServerState

        (standardOutput, errorOutput) <- case statusServerState of
                DevServer.StatusServerNotStarted -> pure ("", "")
                DevServer.StatusServerStarted { standardOutput, errorOutput } -> do
                    std <- readIORef standardOutput
                    err <- readIORef errorOutput
                    pure (std, err)
                DevServer.StatusServerPaused { standardOutput, errorOutput } -> do
                    std <- readIORef standardOutput
                    err <- readIORef errorOutput
                    pure (std, err)

        render LogsView { .. }

    action PostgresLogsAction = do
        currentDevServerState <- readDevServerState
        let postgresState = currentDevServerState |> get #postgresState

        (standardOutput, errorOutput) <- case postgresState of
                DevServer.PostgresStarted { standardOutput, errorOutput } -> do
                    err <- readIORef errorOutput
                    std <- readIORef standardOutput
                    pure (std, err)
                _ -> pure ("", "")

        render LogsView { .. }

readDevServerState :: (?controllerContext :: ControllerContext) => IO DevServer.AppState
readDevServerState = theDevServerContext
        |> get #appStateRef
        |> readIORef

theDevServerContext :: (?controllerContext :: ControllerContext) => DevServer.Context
theDevServerContext = (fromControllerContext @ToolServerApplication) |> get #devServerContext