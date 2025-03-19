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
        toolServerApp <- fromContext @ToolServerApplication

        standardOutput <- cs . ByteString.unlines . reverse <$> readIORef toolServerApp.appStandardOutput
        errorOutput <- cs . ByteString.unlines . reverse <$> readIORef toolServerApp.appErrorOutput

        render LogsView { .. }

    action PostgresLogsAction = do
        toolServerApp <- fromContext @ToolServerApplication

        standardOutput <- cs . ByteString.toLazyByteString <$> readIORef toolServerApp.postgresStandardOutput
        errorOutput <- cs . ByteString.toLazyByteString <$> readIORef toolServerApp.postgresErrorOutput

        render LogsView { .. }

    action OpenEditorAction = do
        let path = param @Text "path"
        let line = paramOrDefault @Int 0 "line"
        let col = paramOrDefault @Int 0 "col"
        openEditor path line col

        renderPlain ""