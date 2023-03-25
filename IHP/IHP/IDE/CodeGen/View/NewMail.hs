module IHP.IDE.CodeGen.View.NewMail where

import IHP.ViewPrelude
import IHP.IDE.ToolServer.Types
import IHP.IDE.CodeGen.Types
import IHP.IDE.CodeGen.View.Generators (renderPlan)

data NewMailView = NewMailView
    { plan :: Either Text [GeneratorAction]
    , mailName :: Text
    , controllerName :: Text
    , applicationName :: Text
    , controllers :: [Text]
    , applications :: [Text]
    }

instance View NewMailView where
    html NewMailView { .. } = [hsx|
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
            renderEmpty = [hsx|<form method="POST" action={NewMailAction} class="d-flex">
                    {when (length applications /= 1) renderApplicationSelector}
                    <select 
                        name="controllerName"
                        class="form-control select2-simple"
                        size="1"
                    >
                        {renderControllerOptions}
                    </select>
                    <input
                        type="text"
                        name="name"
                        placeholder="Mail name"
                        class="form-control"
                        autofocus="autofocus"
                        value={mailName}
                        />
                    <button class="btn btn-primary" type="submit">Preview</button>
                </form>|]
            renderControllerOptions = forM_ controllers (\x -> [hsx|<option>{x}</option>|])
            renderApplicationOptions = forM_ applications (\x -> [hsx|<option selected={x == applicationName}>{x}</option>|])
            renderApplicationSelector = [hsx|
                <select
                    name="applicationName"
                    class="form-control select2-simple"
                    size="1"
                >
                    {renderApplicationOptions}
                </select>|]
            renderPreview = [hsx|
                <form method="POST" action={CreateMailAction} class="d-flex">
                    <div class="object-name flex-grow-1">{controllerName}.Mail.{mailName}</div>

                    <input type="hidden" name="name" value={mailName}/>
                    <input type="hidden" name="controllerName" value={controllerName}/>
                    <input type="hidden" name="applicationName" value={applicationName}/>

                    <button class="btn btn-primary" type="submit">Generate</button>
                </form>
            |]

            isEmpty = null mailName    
