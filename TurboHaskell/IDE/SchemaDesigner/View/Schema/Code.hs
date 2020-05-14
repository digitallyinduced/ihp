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
    beforeRender (context, view) = (context { layout = schemaDesignerLayout }, view)

    html CodeView { .. } = [hsx|
        <div class="editor-container">
            <div id="editor">{preEscapedToHtml schema}</div>
        </div>
        {errorDiv}
        <div class="toolbox">
            <button id="save-button" class="btn btn-primary">Save</button>
        </div>
    |]
        where
            errorDiv = case error of
                Nothing -> mempty
                Just error -> preEscapedToHtml [plain|
                        <div class="error-box">
                            <pre class="text-white p-5">#{error}</pre>
                        </div>
                    |]
