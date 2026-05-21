module IHP.IDE.Logs.Controller where

import IHP.ControllerPrelude
import IHP.IDE.ToolServer.Helper.Controller
import IHP.IDE.ToolServer.Types
import IHP.IDE.Logs.View.Logs
import IHP.IDE.Logs.ServiceLog (discoverServices, getServiceLogs)
import qualified Data.ByteString.Char8 as ByteString
import qualified IHP.EnvVar as EnvVar
import qualified System.Directory as Directory

instance Controller LogsController where
    action AppLogsAction = do
        let toolServerApp = theToolServerApplication

        standardOutput <- cs . ByteString.unlines . reverse <$> readIORef toolServerApp.appStandardOutput
        errorOutput <- cs . ByteString.unlines . reverse <$> readIORef toolServerApp.appErrorOutput

        services <- discoverServices
        let activeService = "app"

        render LogsView { .. }

    action PostgresLogsAction = do
        pgdata <- EnvVar.env @String "PGDATA"
        let logFile = pgdata <> "/log/postgresql.log"

        logExists <- Directory.doesFileExist logFile
        standardOutput <- if logExists
            then cs <$> ByteString.readFile logFile
            else pure ("Postgres log file not found" :: ByteString)
        let errorOutput = "" :: ByteString

        services <- discoverServices
        let activeService = "postgres"

        render LogsView { .. }

    action ServiceLogsAction { serviceName } = do
        standardOutput <- cs <$> getServiceLogs serviceName
        let errorOutput = "" :: ByteString

        services <- discoverServices
        let activeService = serviceName

        render LogsView { .. }

    action OpenEditorAction = do
        let path = param @Text "path"
        let line = paramOrDefault @Int 0 "line"
        let col = paramOrDefault @Int 0 "col"
        openEditor path line col

        renderPlain ""
