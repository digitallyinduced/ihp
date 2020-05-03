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
        {codeNav}
        {editor}
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