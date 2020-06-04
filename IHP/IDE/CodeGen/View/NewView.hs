module IHP.IDE.CodeGen.View.NewView where

import IHP.ViewPrelude
import IHP.IDE.SchemaDesigner.Types
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Layout
import IHP.View.Modal
import IHP.IDE.SchemaDesigner.View.Layout
import IHP.IDE.CodeGen.Types
import IHP.IDE.CodeGen.View.Generators (renderPlan)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

data NewViewView = NewViewView
    { plan :: Either Text [GeneratorAction]
    , viewName :: Text
    , controllerName :: Text
    , listOfControllers :: [Text]
    }

instance View NewViewView ViewContext where
    html NewViewView { .. } = [hsx|
        <div class="generators">
            <div class="container pt-5">
                <div class="code-generator new-script">
                    {if isEmpty then renderEmpty else renderPreview}
                    {unless isEmpty (renderPlan plan)}
                </div>
            </div>
        </div>
    |]
        where
            renderEmpty = [hsx|<form method="POST" action={NewViewAction} class="d-flex">
                    <input
                        type="text"
                        name="name"
                        placeholder="View name"
                        class="form-control"
                        autofocus="autofocus"
                        value={viewName}
                        />
                    <select 
                        name="controllerName"
                        class="form-control"
                        size="1"
                    >
                        <option value="" disabled="disabled" selected="selected">controllerName</option>
                        {renderOptions}
                    </select>
                    <button class="btn btn-primary" type="submit">Preview</button>
                </form>|]
            renderOptions = forM_ listOfControllers (\x -> [hsx|<option>{x}</option>|])
            renderPreview = [hsx|
                <form method="POST" action={CreateViewAction} class="d-flex">
                    <div class="object-name flex-grow-1">{viewName}</div>

                    <input type="hidden" name="name" value={viewName}/>

                    <button class="btn btn-primary" type="submit">Generate</button>
                </form>
            |]

            isEmpty = null viewName    