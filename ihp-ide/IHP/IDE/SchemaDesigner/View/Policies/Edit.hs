module IHP.IDE.SchemaDesigner.View.Policies.Edit where

import IHP.ViewPrelude
import IHP.Postgres.Types
import IHP.IDE.ToolServer.Types
import IHP.IDE.SchemaDesigner.View.Layout
import IHP.IDE.SchemaDesigner.View.Policies.New (policyFormModal)

data EditPolicyView = EditPolicyView
    { statements :: [Statement]
    , tableName :: Text
    , columns :: [Column]
    , policy :: Statement
    }

instance View EditPolicyView where
    html EditPolicyView { .. } = [hsx|
        <div class="row g-0 bg-white" id="schema-designer-viewer">
            {renderObjectSelector (zip [0..] statements) (Just tableName)}
            {renderColumnSelector tableName (zip [0..] columns) statements}
        </div>
        {migrationStatus}
        {renderModal modal}
    |]
        where
            extraHiddenFields = [hsx|
                <!--
                    The hidden name field is required as the user could be changing the name and we don't have any other
                    identifier to refer to the policy besides the name
                -->
                <input type="hidden" name="name" value={policy.name}/>
            |]
            modal = policyFormModal tableName columns policy (pathTo UpdatePolicyAction) extraHiddenFields "Update Policy" "Edit Policy"
