module IHP.IDE.SchemaDesigner.View.Migrations.New where
import IHP.ViewPrelude
import IHP.IDE.ToolServer.Helper.View
import IHP.SchemaMigration
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Routes
import IHP.IDE.CodeGen.Types

data NewView = NewView { plan :: [GeneratorAction] }

instance View NewView where
    html NewView { .. } = [hsx|
        <div class="container pt-5">
            <h1>New Migration</h1>
            <div class="migration pending">
                {renderForm sqlStatements}
            </div>
        </div>
    |]
        where
            sqlStatements = plan
                |> mapMaybe \case
                    CreateFile { fileContent } -> Just fileContent
                    _ -> Nothing
                |> headMay
                |> fromMaybe ""


renderForm :: Text -> Html
renderForm sqlStatements = [hsx|
<form method="POST" action={CreateMigrationAction}>
    <div class="form-group" id="form-group-migration_sqlStatements">
        <textarea name="sqlStatements" placeholder="" id="migration_sqlStatements" class="form-control">{sqlStatements}</textarea>
    </div>
    <div id="migration_sqlStatements_ace"/>

    <div class="d-flex">
        <button class="btn btn-primary">
            <span id="addMigrationVerb">Run</span> Migration
        </button>

        <div class="ml-3 d-flex align-items-center">
            <div class="custom-control custom-switch d-flex align-items-center">
                <input type="checkbox" class="custom-control-input" id="createOnlySwitch" name="createOnly" onchange="addMigrationVerb.innerText = this.checked ? 'Add' : 'Run'"/>
                <label class="custom-control-label text-muted" for="createOnlySwitch" style="font-size: 14px">Create only, don't run yet</label>
            </div>
        </div>
    </div>
</form>
|]