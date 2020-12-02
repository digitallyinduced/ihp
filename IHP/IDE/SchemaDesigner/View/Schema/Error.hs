module IHP.IDE.SchemaDesigner.View.Schema.Error where

import IHP.ViewPrelude
import IHP.IDE.SchemaDesigner.Types
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Layout
import IHP.IDE.SchemaDesigner.View.Layout

data ErrorView = ErrorView
    { error :: ByteString
    }

instance View ErrorView where
    html ErrorView { .. } = [hsx|
        <div class="container">
            <div class="row pt-5" style="height: 102px">
                <div class="col" style="display: flex; align-self: center;">
                    {visualNav}
                </div>

                <div class="col" style="display: flex; align-self: center; justify-content: center">
                    Application/Schema.sql
                </div>

                <div class="col"></div>
            </div>
            <div>
                <div class="bg-white visual-error">
                    <pre>{error}</pre>
                </div>
            </div>
        </div>
    |]