module IHP.IDE.SchemaDesigner.View.EnumValues.Edit where

import IHP.ViewPrelude
import IHP.IDE.SchemaDesigner.Types
import IHP.IDE.ToolServer.Types
import IHP.IDE.SchemaDesigner.View.Layout

data EditEnumValueView = EditEnumValueView
    { statements :: [Statement]
    , enumName :: Text
    , valueId :: Int
    , value :: Text
    }

instance View EditEnumValueView where
    html EditEnumValueView { .. } = [hsx|
        <div class="row no-gutters bg-white" id="schema-designer-viewer">
            {renderObjectSelector (zip [0..] statements) (Just enumName)}
            {renderEnumSelector enumName (zip [0..] values)}
        </div>
        {renderModal modal}
    |]
        where
            enum = findStatementByName enumName statements
            values = maybe [] (get #values) enum
            
            modalContent = [hsx|
                <form method="POST" action={UpdateEnumValueAction}>
                    <input type="hidden" name="enumName" value={enumName}/>
                    <input type="hidden" name="valueId" value={tshow valueId}/>

                    <div class="form-group row">
                        <label class="col-sm-2 col-form-label">Name:</label>
                        <div class="col-sm-10">
                            <input name="enumValueName" type="text" class="form-control" autofocus="autofocus" value={value}/>
                        </div>
                    </div>

                    <div class="text-right">
                        <button type="submit" class="btn btn-primary">Edit Enum Value</button>
                    </div>
                </form>
            |]
            modalFooter = mempty 
            modalCloseUrl = pathTo ShowEnumAction { enumName }
            modalTitle = "Edit Enum Value"
            modal = Modal { modalContent, modalFooter, modalCloseUrl, modalTitle }
