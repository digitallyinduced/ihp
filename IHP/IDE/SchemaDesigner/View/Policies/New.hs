module IHP.IDE.SchemaDesigner.View.Policies.New where

import IHP.ViewPrelude
import IHP.IDE.SchemaDesigner.Types
import qualified IHP.IDE.SchemaDesigner.Compiler as Compiler
import IHP.IDE.ToolServer.Types
import IHP.IDE.SchemaDesigner.View.Layout

data NewPolicyView = NewPolicyView
    { statements :: [Statement]
    , tableName :: Text
    , columns :: [Column]
    , policy :: Statement
    }

instance View NewPolicyView where
    html NewPolicyView { .. } = [hsx|
        <div class="row no-gutters bg-white" id="schema-designer-viewer">
            {renderObjectSelector (zip [0..] statements) (Just tableName)}
            {renderColumnSelector tableName (zip [0..] columns) statements}
        </div>
        {renderModal modal}
    |]
        where
            modalContent = [hsx|
                <form method="POST" action={CreatePolicyAction} class="edit-policy">
                    <input type="hidden" name="tableName" value={tableName}/>

                    <!-- These will be filled via JS from the ace editors -->
                    <input type="hidden" name="using" value={using}/>
                    <input type="hidden" name="check" value={check}/>

                    <div class="form-group">
                        <input
                            id="nameInput"
                            name="policyName"
                            type="text"
                            class="form-control"
                            autofocus="autofocus"
                            value={get #name policy}
                            />
                    </div>

                    <div class="form-group">
                        <label for="using">Visible if:</label>
                        <textarea
                            id="using"
                            name="using"
                            type="text"
                            class="form-control sql-expression"
                            data-autocomplete-suggestions={autocompleteSuggestions}
                        >{using}</textarea>
                        <small class="form-text text-muted">This SQL expression needs to return True if the row should be visible to the current user. This is the <code>USING</code> condition of the Postgres Policy</small>
                    </div>

                    <div class="form-group">
                        <label for="using">Additionally, allow INSERT and UPDATE only if:</label>
                        <textarea
                            id="check"
                            name="check"
                            type="text"
                            class="form-control sql-expression"
                            data-autocomplete-suggestions={autocompleteSuggestions}
                        >{check}</textarea>
                        <small class="form-text text-muted">Use this to e.g. disallow users changing the user_id to another user's id. This is the <code>CHECK</code> condition of the Postgres Policy</small>
                    </div>

                    <div class="text-right">
                        <button type="submit" class="btn btn-primary">Create Policy</button>
                    </div>
                </form>
            |]
            modalFooter = mempty
            modalCloseUrl = pathTo ShowTableAction { tableName }
            modalTitle = "New Policy"
            modal = Modal { modalContent, modalFooter, modalCloseUrl, modalTitle }

            using = get #using policy
                    |> maybe "" Compiler.compileExpression

            check = get #check policy
                    |> maybe "" Compiler.compileExpression

            autocompleteSuggestions =
                    columns
                    |> map (get #name)
                    |> intercalate ","