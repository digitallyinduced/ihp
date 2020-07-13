module IHP.IDE.CodeGen.View.NewAction where

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

data NewActionView = NewActionView
    { plan :: Either Text [GeneratorAction]
    , actionName :: Text
    , controllerName :: Text
    , applicationName :: Text
    , doGenerateView :: Bool
    , controllers :: [Text]
    , applications :: [Text]
    }

instance View NewActionView ViewContext where
    html NewActionView { .. } = [hsx|
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
            renderEmpty = [hsx|<form method="POST" action={NewActionAction}>
                    <div class="d-flex">
                        <select
                            name="applicationName"
                            class="form-control select2-simple"
                            size="1"
                            onchange="this.form.submit()"
                        >
                            {renderApplicationOptions}
                        </select>
                        <select 
                            name="controllerName"
                            class="form-control select2-simple"
                            size="1"
                        >
                            {renderOptions}
                        </select>
                        <input
                            type="text"
                            name="name"
                            placeholder="Action name"
                            class="form-control"
                            autofocus="autofocus"
                            value={actionName}
                            />
                        <button class="btn btn-primary" type="submit">Preview</button>
                    </div>
                    <div class="generator-options">
                        <input
                            type="checkbox"
                            name="doGenerateView"
                            id="doGenerateView"
                            />
                        <label class="pl-1" for="doGenerateView">With View</label>
                    </div>
                </form>|]
            renderControllerOptions = forM_ controllers (\x -> [hsx|<option>{x}</option>|])
            renderApplicationOptions = forM_ applications (\x -> [hsx|<option selected={x == applicationName}>{x}</option>|])
            renderPreview = [hsx|
                <form method="POST" action={CreateActionAction} class="d-flex">
                    <div class="object-name flex-grow-1">{controllerName}.{actionName}</div>

                    <input type="hidden" name="name" value={actionName}/>
                    <input type="hidden" name="controllerName" value={controllerName}/>

                    <button class="btn btn-primary" type="submit">Generate</button>
                </form>
                <div class="generator-options">
                    <form method="POST" action={NewActionAction}>
                        <input
                            type="checkbox"
                            name="doGenerateView"
                            id="doGenerateView"
                            checked = {doGenerateView}
                            onchange = "this.form.submit()"
                            />
                        <label class="pl-1" for="doGenerateView">With View</label>
                        <input type="hidden" name="name" value={actionName}/>
                        <input type="hidden" name="controllerName" value={controllerName}/>
                    </form>
                </div>
            |]

            isEmpty = null actionName    
