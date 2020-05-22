module IHP.IDE.SchemaDesigner.View.EnumValues.New where

import IHP.ViewPrelude
import IHP.IDE.SchemaDesigner.Types
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Layout
import IHP.View.Modal
import IHP.IDE.SchemaDesigner.View.Layout

data NewEnumValueView = NewEnumValueView
    { statements :: [Statement]
    , enumName :: Text
    }

instance View NewEnumValueView ViewContext where
    html NewEnumValueView { .. } = [hsx|
        {visualNav}
        <div class="container">
            <div class="d-flex justify-content-end col">
                <form class="p-2" action={pathTo DumpDbAction}>
                    <button type="submit" class="btn btn-primary">Dump DB</button>
                </form>
                <form class="p-2" style="padding-right: 0 !important;" action={pathTo PushToDbAction}>
                    <button type="submit" class="btn btn-primary">Push to DB</button>
                </form>
            </div>
            <div class="row no-gutters bg-white">
                {renderObjectSelector (zip [0..] statements) (Just enumName)}
                {renderEnumSelector enumName (zip [0..] values)}
            </div>
        </div>
        {Just modal}
    |]
        where
            table = findEnumByName enumName statements
            values = maybe [] (get #values) table

            modalContent = [hsx|
                <form method="POST" action={CreateEnumValueAction}>
                    <input type="hidden" name="enumName" value={enumName}/>

                    <div class="form-group row">
                        <label class="col-sm-2 col-form-label">Name:</label>
                        <div class="col-sm-10">
                            <input name="enumValueName" type="text" class="form-control" autofocus="autofocus"/>
                        </div>
                    </div>

                    <div class="text-right">
                        <button type="submit" class="btn btn-primary">Create Enum Value</button>
                    </div>
                </form>
            |]
            modalFooter = mempty 
            modalCloseUrl = pathTo ShowEnumAction { enumName }
            modalTitle = "New Enum Value"
            modal = Modal { modalContent, modalFooter, modalCloseUrl, modalTitle }
