module IHP.IDE.CodeGen.View.NewMigration where

import IHP.ViewPrelude
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Layout
import IHP.IDE.CodeGen.Types
import IHP.IDE.CodeGen.View.Generators (renderPlan)

data NewMigrationView = NewMigrationView
    { description :: Text
    , plan :: Either Text [GeneratorAction]
    , runMigration :: Bool
    }

instance View NewMigrationView where
    html NewMigrationView { .. } = [hsx|
        <div class="generators">
            {renderFlashMessages}
            <div class="container pt-5">
                <div class="code-generator new-migration">
                    {renderForm}
                    {renderPlan plan}
                </div>
            </div>
        </div>
    |]
        where
            renderForm = [hsx|
                <form method="POST" action={CreateMigrationAction}>
                    <input type="hidden" name="runMigration" value={inputValue runMigration}/>
                    <div class="d-flex">
                        <input
                            type="text"
                            name="description"
                            placeholder="Migration description"
                            class="form-control"
                            autofocus="autofocus"
                            id="description"
                            value={description}
                            />

                        <button class="btn btn-primary" type="submit">{submitText}</button>
                    </div>
                </form>
                
                <div class="generator-options">
                    <form method="POST" action={NewMigrationAction}>
                        <input
                            type="checkbox"
                            name="runMigration"
                            id="runMigration"
                            checked={runMigration}
                            onchange="this.form.description.value = window.description.value; this.form.submit()"
                        />
                        <input type="hidden" name="runMigration" value={inputValue False}/>
                        <input type="hidden" name="description" value=""/>
                        <label class="pl-1" for="runMigration">Run Migration after <q>Generate</q></label>
                    </form>
                </div>
            |]

            submitText :: Text
            submitText = if runMigration
                    then "Generate & Run"
                    else "Generate"