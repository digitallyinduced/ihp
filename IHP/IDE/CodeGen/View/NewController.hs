module IHP.IDE.CodeGen.View.NewController where

import IHP.ViewPrelude
import IHP.IDE.CodeGen.Types
import IHP.IDE.ToolServer.Types
import IHP.IDE.CodeGen.View.Generators (renderPlan)

data NewControllerView = NewControllerView
    { plan :: Either Text [GeneratorAction]
    , controllerName :: Text
    , applicationName :: Text
    , applications :: [Text]
    , pagination :: Bool
    }

instance View NewControllerView where
    html NewControllerView { .. } = [hsx|
        <div class="generators">
            {renderFlashMessages}
            <div class="container pt-5">
                <div class="code-generator new-controller">
                    {if isEmpty then renderEmpty else renderPreview}
                    {unless isEmpty (renderPlan plan)}
                </div>
            </div>
        </div>
    |]
        where
            renderEmpty = [hsx|
                <form method="POST" action={NewControllerAction} class="d-flex">
                    {when (length applications /= 1) renderApplicationSelector}
                    <input
                        type="text"
                        name="name"
                        placeholder="Controller name"
                        class="form-control"
                        autofocus="autofocus"
                        value={controllerName}
                        />
                    <button class="btn btn-primary" type="submit">Preview</button>
                </form>|]

            renderPreview = [hsx|
                <form method="POST" action={CreateControllerAction} class="d-flex">
                    <div class="object-name flex-grow-1">{controllerName}</div>

                    <input type="hidden" name="name" value={controllerName}/>
                    <input type="hidden" name="applicationName" value={applicationName}/>
                    <input type="hidden" name="pagination" value={inputValue pagination}/>

                    <button class="btn btn-primary" type="submit">Generate</button>
                </form>

                <div class="generator-options">
                    <form method="POST" action={NewControllerAction}>
                        <input type="hidden" name="name" value={controllerName}/>
                        <input type="hidden" name="applicationName" value={applicationName}/>
                        <input
                            type="checkbox"
                            name="pagination"
                            id="pagination"
                            checked={pagination}
                            onchange="this.form.submit()"
                            />
                        <label class="pl-1" for="pagination">Pagination</label>
                    </form>
                </div>
            |]
            renderApplicationOptions = forM_ applications (\x -> [hsx|<option selected={x == applicationName}>{x}</option>|])
            renderApplicationSelector = [hsx|
                <select
                    name="applicationName"
                    class="form-control select2-simple"
                    size="1"
                >
                    {renderApplicationOptions}
                </select>|]
            isEmpty = null controllerName
