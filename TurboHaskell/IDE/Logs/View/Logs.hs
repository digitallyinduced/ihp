module TurboHaskell.IDE.Logs.View.Logs where

import TurboHaskell.ViewPrelude
import TurboHaskell.IDE.ToolServer.Types
import TurboHaskell.IDE.ToolServer.Layout

data LogsView = LogsView { standardOutput :: ByteString, errorOutput :: ByteString }

instance View LogsView ViewContext where
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
