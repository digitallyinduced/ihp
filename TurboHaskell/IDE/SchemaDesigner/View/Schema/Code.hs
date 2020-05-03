module TurboHaskell.IDE.SchemaDesigner.View.Schema.Code where

import TurboHaskell.ViewPrelude
import TurboHaskell.IDE.SchemaDesigner.Types
import TurboHaskell.IDE.ToolServer.Types
import TurboHaskell.IDE.ToolServer.Layout
import TurboHaskell.View.Modal
import TurboHaskell.IDE.SchemaDesigner.View.Layout

data CodeView = CodeView {
    schema :: Text
}

instance View CodeView ViewContext where
    html CodeView { .. } = [hsx|
        <ul class="nav nav-tabs bg-white" id="myTab" role="tablist">
            <li class="nav-item">
                <a class="nav-link" href={pathTo TablesAction}>Visual Editor</a>
            </li>
            <li class="nav-item">
                <a class="nav-link active">Code Editor</a>
            </li>
            <div class="toolbox">
                <button id="save-button" class="btn btn-primary">Save</button>
            </div>
        </ul>
        {editor}
        {saveScript}
    |]
        where
            editor = preEscapedToHtml [plain|
                <div id="editor">#{schema}</div>
                <script src="/vendor/src-min/ace.js" type="text/javascript" charset="utf-8"></script>
                <script src="/vendor/src-min/ext-language_tools.js"></script>
                <script>
                    ace.require("ace/ext/language_tools");
                    var editor = ace.edit("editor");
                    editor.setTheme("ace/theme/solarized_dark");
                    editor.session.setMode("ace/mode/sql");
                    editor.setOptions({
                        enableBasicAutocompletion: true,
                        enableLiveAutocompletion: true
                    });
                </script>
            |]
            saveScript = preEscapedToHtml [plain|
                <script>
                    $('#save-button').click(function saveSchema() {
                        var form = document.createElement('form');
                        form.action = "http://localhost:8001/turbohaskell/SaveCode";
                        form.method = 'POST';

                        var methodInput = document.createElement('input');
                        console.log(form.action);
                        methodInput.type = 'hidden';
                        methodInput.name = 'schemaSql';
                        methodInput.value = editor.getValue();

                        form.appendChild(methodInput);

                        document.body.appendChild(form);
                        window.submitForm(form);
                    });
                </script>
            |]