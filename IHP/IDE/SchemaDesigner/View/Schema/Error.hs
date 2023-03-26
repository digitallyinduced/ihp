module IHP.IDE.SchemaDesigner.View.Schema.Error where

import IHP.ViewPrelude
import IHP.IDE.SchemaDesigner.View.Layout

data ErrorView = ErrorView
    { error :: ByteString
    }

instance View ErrorView where
    html ErrorView { .. } = [hsx|
        <div class="visual-error">
            <pre><code>{error}</code></pre>
        </div>
    |]
