module IHP.IDE.SchemaDesigner.View.Schema.Code where

import IHP.ViewPrelude
import IHP.IDE.SchemaDesigner.Types
import IHP.IDE.ToolServer.Types
import IHP.IDE.SchemaDesigner.View.Layout

data CodeView = CodeView
    { schema :: Text
    , error :: Maybe ByteString
    }

instance View CodeView where
    html CodeView { .. } = [hsx|
        <div class="editor-container h-100" style="position: relative">
            <div class="toolbox" style="position: absolute; top: 16px; right: 16px; z-index: 10000">
                <button id="save-button" class="btn btn-secondary" data-submit-url={submitUrl} style="transition: all 0.2s">Save</button>
            </div>
            <div id="editor" class="h-100">
                <div class="code-editor-container h-100"></div>
                <textarea class="source-code d-none">{preEscapedToHtml schema}</textarea>
            </div>
        </div>
        {errorDiv}
    |]
        where
            submitUrl = pathTo SaveCodeAction
            errorDiv = case error of
                Nothing -> mempty
                Just error -> preEscapedToHtml [plain|
                        <div class="error-box">
                            <pre class="text-white p-5">#{error}</pre>
                        </div>
                    |]
