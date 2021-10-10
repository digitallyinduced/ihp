module IHP.IDE.SchemaDesigner.View.Schema.Code where

import IHP.ViewPrelude
import IHP.IDE.SchemaDesigner.Types

data CodeView = CodeView
    { schema :: Text
    , error :: Maybe ByteString
    }

instance View CodeView where
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
