module IHP.IDE.CodeGen.View.NewJob where

import IHP.ViewPrelude
import IHP.IDE.SchemaDesigner.Types
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Layout
import IHP.IDE.SchemaDesigner.View.Layout
import IHP.IDE.CodeGen.Types
import IHP.IDE.CodeGen.View.Generators (renderPlan)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

data NewJobView = NewJobView
    { plan :: Either Text [GeneratorAction]
    , jobName :: Text
    , applicationName :: Text
    , applications :: [Text]
    }

instance View NewJobView where
    html NewJobView { .. } = [hsx|
        <div class="generators">
            {renderFlashMessages}
            <div class="container pt-5">
                <div class="code-generator new-script">
                    {if isEmpty jobName then renderEmpty else renderPreview}
                    {unless (isEmpty jobName) (renderPlan plan)}
                </div>
            </div>
        </div>
    |]
        where
            renderEmpty = [hsx|
                <form method="POST" action={NewJobAction} class="d-flex">
                    {when (length applications /= 1) renderApplicationSelector}
                    <input
                        type="text"
                        name="name"
                        placeholder="Job name"
                        class="form-control"
                        autofocus="autofocus"
                        value={jobName}
                        />
                    <button class="btn btn-primary" type="submit">Preview</button>
                </form>
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
            renderPreview = [hsx|
                <form method="POST" action={CreateJobAction} class="d-flex">
                    <div class="object-name flex-grow-1">{applicationName}.Job.{jobName}</div>

                    <input type="hidden" name="name" value={jobName}/>
                    <input type="hidden" name="applicationName" value={applicationName}/>

                    <button class="btn btn-primary" type="submit">Generate</button>
                </form>
            |]
