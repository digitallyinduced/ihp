module TurboHaskell.IDE.SchemaDesigner.View.Schema.GeneratedCode where

import TurboHaskell.ViewPrelude
import TurboHaskell.IDE.SchemaDesigner.Types
import TurboHaskell.IDE.ToolServer.Types
import TurboHaskell.IDE.ToolServer.Layout
import TurboHaskell.View.Modal
import TurboHaskell.IDE.SchemaDesigner.View.Layout

data GeneratedCodeView = GeneratedCodeView
    { statements :: [Statement]
    , generatedHaskellCode :: Text
    }

instance View GeneratedCodeView ViewContext where
    html GeneratedCodeView { .. } = [hsx|
        {visualNav}
        <div class="container">
            <form class="w-100 d-flex justify-content-end" action={pathTo PushToDbAction}>
                <button type="submit" class="btn btn-primary my-3">Push to DB</button>
            </form>
            <div class="row no-gutters bg-white">
                {renderObjectSelector (zip [0..] statements) Nothing}
            </div>
        </div>
        {Just modal}
        {customCss}
    |]
        where
            modalContent = [hsx|
                <pre class="generated-haskell-code"><code>{generatedHaskellCode}</code></pre>
            |]
            modalFooter = mempty
            modalCloseUrl = pathTo TablesAction
            modalTitle = "Generated Haskell Code"
            modal = Modal { modalContent, modalFooter, modalCloseUrl, modalTitle }
            customCss = preEscapedToHtml [plain|
            <style>
                #modal-inner {
                    max-width: 90vw !important;
                }
            </style>|]
