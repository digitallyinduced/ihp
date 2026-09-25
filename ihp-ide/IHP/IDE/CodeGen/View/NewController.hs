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
    , indexAction :: Bool
    , newAction :: Bool
    , showAction :: Bool
    , createAction :: Bool
    , editAction :: Bool
    , updateAction :: Bool
    , deleteAction :: Bool
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
                    <input type="hidden" name="indexAction" value={inputValue indexAction}/>
                    <input type="hidden" name="newAction" value={inputValue newAction}/>
                    <input type="hidden" name="showAction" value={inputValue showAction}/>
                    <input type="hidden" name="createAction" value={inputValue createAction}/>
                    <input type="hidden" name="editAction" value={inputValue editAction}/>
                    <input type="hidden" name="updateAction" value={inputValue updateAction}/>
                    <input type="hidden" name="deleteAction" value={inputValue deleteAction}/>

                    <button class="btn btn-primary" type="submit">Generate</button>
                </form>

                <div class="generator-options">
                    <form method="POST" action={NewControllerAction}>
                        <input type="hidden" name="name" value={controllerName}/>
                        {renderApplicationNameSelector}
                        <input type="checkbox" name="pagination" id="pagination" checked={pagination} onchange="this.form.submit()"/>
                        <label class="ps-1" for="pagination">Pagination</label>

                        <span class="ms-3">Actions:</span>
                        {renderActionCheckbox "indexAction" "Index" indexAction}
                        {renderActionCheckbox "newAction" "New" newAction}
                        {renderActionCheckbox "showAction" "Show" showAction}
                        {renderActionCheckbox "createAction" "Create" createAction}
                        {renderActionCheckbox "editAction" "Edit" editAction}
                        {renderActionCheckbox "updateAction" "Update" updateAction}
                        {renderActionCheckbox "deleteAction" "Delete" deleteAction}
                    </form>
                </div>
            |]
            renderActionCheckbox :: Text -> Text -> Bool -> Html
            renderActionCheckbox name label checked = [hsx|
                <input type="checkbox" name={name} id={name} checked={checked} onchange="this.form.submit()"/>
                <input type="hidden" name={name} value={inputValue False}/>
                <label class="ps-1 me-2" for={name}>{label}</label>
            |]
            renderApplicationNameSelector = if length applications /= 1
                then renderApplicationSelector
                else [hsx|<input type="hidden" name="applicationName" value={applicationName}/>|]
            renderApplicationOptions = forM_ applications (\x -> [hsx|<option selected={x == applicationName}>{x}</option>|])
            renderApplicationSelector = [hsx|
                <select
                    name="applicationName"
                    class="form-control select2-simple"
                    size="1"
                    onchange="this.form.submit()"
                >
                    {renderApplicationOptions}
                </select>|]
            isEmpty = null controllerName
