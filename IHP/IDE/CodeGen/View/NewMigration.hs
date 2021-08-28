module IHP.IDE.CodeGen.View.NewMigration where

import IHP.ViewPrelude
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Layout

data NewMigrationView = NewMigrationView { description :: Text }

instance View NewMigrationView where
    html NewMigrationView { .. } = [hsx|
        <div class="generators">
            {renderFlashMessages}
            <div class="container pt-5">
                <div class="code-generator new-script">
                    {renderForm}
                </div>
            </div>
        </div>
    |]
        where
            renderForm = [hsx|
                <form method="POST" action={CreateMigrationAction} class="d-flex">
                    <input
                        type="text"
                        name="description"
                        placeholder="Migration description"
                        class="form-control"
                        autofocus="autofocus"
                        value={description}
                        />

                    <button class="btn btn-primary" type="submit">Generate</button>
                </form>
            |]
