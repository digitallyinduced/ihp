module IHP.IDE.SchemaDesigner.View.Migrations.New where
import IHP.ViewPrelude
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Routes ()
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
    <div class="mb-3" id="form-group-migration_sqlStatements">
        <textarea name="sqlStatements" placeholder="" id="migration_sqlStatements" class="form-control">{sqlStatements}</textarea>
    </div>
    <div id="migration_sqlStatements_ace"/>

    <div class="d-flex">
        <button class="btn btn-primary">
            <span id="addMigrationVerb">Run</span> Migration
        </button>

        <div class="ms-3 d-flex align-items-center">
            <div class="form-check form-switch d-flex align-items-center">
                <input type="checkbox" class="form-check-input" id="createOnlySwitch" name="createOnly" onchange="addMigrationVerb.innerText = this.checked ? 'Add' : 'Run'"/>
                <label class="form-check-label text-muted" for="createOnlySwitch" style="font-size: 14px">Create only, don't run yet</label>
            </div>
        </div>
    </div>
</form>
|]
