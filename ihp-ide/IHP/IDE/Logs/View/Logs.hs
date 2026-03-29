module IHP.IDE.Logs.View.Logs where

import IHP.ViewPrelude
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Layout ()

data LogsView = LogsView
    { standardOutput :: ByteString
    , errorOutput :: ByteString
    , services :: [Text]
    , activeService :: Text
    }

instance View LogsView where
    html LogsView { .. } = [hsx|
        <div id="logs">
            <div class="logs-tab-bar">
                <a href={AppLogsAction} class={classes [("logs-tab", True), ("active", activeService == "app")]}>App</a>
                <a href={PostgresLogsAction} class={classes [("logs-tab", True), ("active", activeService == "postgres")]}>Postgres</a>
                {forEach services serviceTab}
            </div>

            <div class="logs-output">
                <pre>{standardOutput}</pre>
                {renderStderr}
            </div>
        </div>
        <script>
            document.addEventListener('turbo:load', function() {
                var output = document.querySelector('.logs-output');
                if (output) output.scrollTop = output.scrollHeight;
            });
        </script>
    |]
        where
            renderStderr = if errorOutput /= ""
                then [hsx|<pre class="stderr">{errorOutput}</pre>|]
                else mempty

            serviceTab :: Text -> Html
            serviceTab name = [hsx|
                <a href={ServiceLogsAction name} class={classes [("logs-tab", True), ("active", activeService == name)]}>{name}</a>
            |]
