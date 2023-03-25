module IHP.IDE.Data.View.ShowForeignKeyHoverCard where

import IHP.ViewPrelude
import IHP.IDE.SchemaDesigner.Types
import IHP.IDE.ToolServer.Types
import IHP.IDE.Data.View.Layout
import IHP.IDE.ToolServer.Routes

data ShowForeignKeyHoverCardView = ShowForeignKeyHoverCardView
    { record :: [DynamicField]
    , foreignTableName :: Text
    }

instance View ShowForeignKeyHoverCardView where
    beforeRender _ = setLayout (\view -> view)
    html ShowForeignKeyHoverCardView { .. } = [hsx| 
        <div class="foreign-key-hovercard">
            <h1>{tableNameToModelName foreignTableName}</h1>
            {forEach record renderField}
        </div>
    |]
        where
            renderField field = [hsx|
                <div class="mb-2">
                    <div style="font-size: 12px; color: rgba(197, 207, 211, 0.8)">{get #fieldName field}:</div>

                    <div>
                        {get #fieldValue field}
                    </div>
                </div>
            |]
