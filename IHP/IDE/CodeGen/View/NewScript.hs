module IHP.IDE.CodeGen.View.NewScript where

import IHP.ViewPrelude
import IHP.IDE.ToolServer.Types
import IHP.IDE.CodeGen.Types
import IHP.IDE.CodeGen.View.Generators (renderPlan)

data NewScriptView = NewScriptView
    { plan :: Either Text [GeneratorAction]
    , scriptName :: Text
    }

instance View NewScriptView where
    html NewScriptView { .. } = [hsx|
        <div class="generators">
            {renderFlashMessages}
            <div class="container pt-5">
                <div class="code-generator new-script">
                    {if isEmpty then renderEmpty else renderPreview}
                    {unless isEmpty (renderPlan plan)}
                </div>
            </div>
        </div>
    |]
        where
            renderEmpty = [hsx|<form method="POST" action={NewScriptAction} class="d-flex">
                    <input
                        type="text"
                        name="name"
                        placeholder="Script name"
                        class="form-control"
                        autofocus="autofocus"
                        value={scriptName}
                        />

                    <button class="btn btn-primary" type="submit">Preview</button>
                </form>|]

            renderPreview = [hsx|
                <form method="POST" action={CreateScriptAction} class="d-flex">
                    <div class="object-name flex-grow-1">{scriptName}</div>

                    <input type="hidden" name="name" value={scriptName}/>

                    <button class="btn btn-primary" type="submit">Generate</button>
                </form>
            |]


            isEmpty = null scriptName
