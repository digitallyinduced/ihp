module TurboHaskell.IDE.SchemaDesigner.View.Schema.Error where

import TurboHaskell.ViewPrelude
import TurboHaskell.IDE.SchemaDesigner.Types
import TurboHaskell.IDE.ToolServer.Types
import TurboHaskell.IDE.ToolServer.Layout
import TurboHaskell.View.Modal
import TurboHaskell.IDE.SchemaDesigner.View.Layout

data ErrorView = ErrorView
    { error :: ByteString
    }

instance View ErrorView ViewContext where
    html ErrorView { .. } = [hsx|
        {visualNav}
        <div class="container">
            <div class="row no-gutters bg-white visual-error">
                <pre>{error}</pre>
            </div>
        </div>
    |]