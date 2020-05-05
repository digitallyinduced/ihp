module TurboHaskell.IDE.SchemaDesigner.View.Schema.Code where

import TurboHaskell.ViewPrelude
import TurboHaskell.IDE.SchemaDesigner.Types
import TurboHaskell.IDE.ToolServer.Types
import TurboHaskell.IDE.ToolServer.Layout
import TurboHaskell.View.Modal
import TurboHaskell.IDE.SchemaDesigner.View.Layout

data CodeView = CodeView
    { schema :: Text
    , error :: Maybe ByteString
    }

instance View CodeView ViewContext where
    html CodeView { .. } = [hsx|
        <ul class="nav nav-tabs bg-white" id="myTab" role="tablist">
            <li class="nav-item">
                <a class="nav-link" href={pathTo TablesAction} target="_self">Visual Editor</a>
            </li>
            <li class="nav-item">
                <a class="nav-link active">Code Editor</a>
            </li>
            <div class="toolbox">
                <button id="save-button" class="btn btn-primary">Save</button>
            </div>
        </ul>
        {editor}
        {errorDiv}
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
                    editor.setShowPrintMargin(false);
                    editor.setOptions({
                        enableBasicAutocompletion: true,
                        enableLiveAutocompletion: true
                    });
                    window.onbeforeunload = confirmExit;
                    function confirmExit() {
                        if (!editor.session.getUndoManager().isClean()) {
                            return "You have unsaved changes. Do you want to leave the Editor?";
                        }
                    }
                </script>
            |]
            errorDiv = case error of
                Nothing -> mempty
                Just error -> preEscapedToHtml [plain|
                        <style>
                            #editor { height: 79% !important; }
                        </style>
                        <div class="error-box">
                            <pre class="text-white p-5">#{error}</pre>
                        </div>
                    |]
            saveScript = preEscapedToHtml [plain|
                <script>
                    var saveButton = document.getElementById("save-button");
                    saveButton.disabled = true;
                    saveButton.addEventListener("click", function saveSchema() {
                        editor.session.getUndoManager().markClean()
                        saveButton.disabled = editor.session.getUndoManager().isClean()
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
                        form.submit();
                    });

                    editor.on("input", function() {
                        saveButton.disabled = editor.session.getUndoManager().isClean()
                    });
                </script>
            |]