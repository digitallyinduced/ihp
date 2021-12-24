module IHP.IDE.Repl.View where

import IHP.ViewPrelude
import IHP.IDE.ToolServer.Types

data ReplView = ReplView
    { replLines :: ![Text]
    , replCommands :: ![Text]
    }

instance View ReplView where
    html ReplView { .. } = [hsx|
        <div id="repl">
            <div>
                <pre id="repl-output">
                </pre>
            </div>

            <div id="repl-form">
                <input id="repl-input" name="repl-cmd" placeholder="Enter GHCi command...">
                <input id="repl-submit" type="submit">
            </div>
        </div>

        <script>
            var helloWorldController = new WebSocket('ws://localhost:8001/ReplWSApp');

            helloWorldController.onopen = function (event) {
                console.log('Connected');
            };

            helloWorldController.onmessage = function (event) {
                console.log(event.data);
            };

            var output = new WebSocket('ws://localhost:8001/ReplOutputApp');
            output.onopen = function (event) {
                console.log('Connected output');
            };
            output.onmessage = function (event) {
                console.log({data: event.data})
                const el = document.getElementById("repl-output");
                el.innerText = String(event.data);
                el.scrollTop = el.scrollHeight;
            };

            $(document).ready(() => {
                $("#repl-submit").click(e => {
                   helloWorldController.send(document.getElementById("repl-input").value);
                });
                $("#repl-input").on("keydown", e => {
                    if (e.keyCode == 13) {
                        const element = document.getElementById("repl-input");
                        helloWorldController.send(element.value);
                        element.value = "";
                    }
                });
            });
        </script>
    |]
