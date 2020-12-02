module IHP.IDE.CodeGen.View.NewApplication where

import IHP.ViewPrelude
import IHP.IDE.SchemaDesigner.Types
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Layout
import IHP.IDE.SchemaDesigner.View.Layout
import IHP.IDE.CodeGen.Types
import IHP.IDE.CodeGen.View.Generators (renderPlan)

data NewApplicationView = NewApplicationView
    { plan :: Either Text [GeneratorAction]
    , applicationName :: Text
    }

instance View NewApplicationView where
    html NewApplicationView { .. } = [hsx|
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
            renderEmpty = [hsx|<form method="POST" action={NewApplicationAction} class="d-flex">
                    <input
                        type="text"
                        name="name"
                        placeholder="Application name"
                        class="form-control"
                        autofocus="autofocus"
                        value={applicationName}
                        />

                    <button class="btn btn-primary" type="submit">Preview</button>
                </form>|]

            renderPreview = [hsx|
                <form method="POST" action={CreateApplicationAction} class="d-flex">
                    <div class="object-name flex-grow-1">{applicationName}</div>

                    <input type="hidden" name="name" value={applicationName}/>

                    <button class="btn btn-primary" type="submit">Generate</button>
                </form>
            |]


            isEmpty = null applicationName
