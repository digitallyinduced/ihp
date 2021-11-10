module IHP.IDE.Logs.View.Logs where

import IHP.ViewPrelude
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Layout ()

data LogsView = LogsView { standardOutput :: LByteString, errorOutput :: LByteString }

instance View LogsView where
    html LogsView { .. } = [hsx|
        <div id="logs">
            <div class="logs-navigation">
                <a href={AppLogsAction} class={classes [("active", isActivePath AppLogsAction)]}>App</a>
                <a href={PostgresLogsAction} class={classes [("active", isActivePath PostgresLogsAction)]}>Postgres</a>
            </div>

            <pre>{standardOutput}</pre>
            <pre>{errorOutput}</pre>
        </div>
    |]
