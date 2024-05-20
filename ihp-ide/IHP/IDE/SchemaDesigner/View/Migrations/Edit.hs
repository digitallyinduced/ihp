module IHP.IDE.SchemaDesigner.View.Migrations.Edit where
import IHP.ViewPrelude
import IHP.SchemaMigration
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Routes ()

data EditView = EditView
    { migration :: Migration
    , sqlStatements :: !Text
    }

instance View EditView where
    html EditView { .. } = [hsx|
        <div class="container pt-5">
            <div class="migration pending">
                {renderForm migration sqlStatements}
            </div>
        </div>
    |]


renderForm :: Migration -> Text -> Html
renderForm migration sqlStatements = [hsx|
<form method="POST" action={UpdateMigrationAction (migration.revision)}>
    <div class="form-group" id="form-group-migration_sqlStatements">
        <textarea name="sqlStatements" placeholder="" id="migration_sqlStatements" class="form-control">{sqlStatements}</textarea>
    </div>
    <div id="migration_sqlStatements_ace"/>

    <button
        class="btn btn-primary"
        data-toggle="tooltip"
        data-placement="right"
        title="⌘ Enter"
        >Save Migration</button>
</form>
|]
