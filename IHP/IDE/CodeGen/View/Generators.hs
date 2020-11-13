module IHP.IDE.CodeGen.View.Generators where

import IHP.ViewPrelude
import IHP.IDE.SchemaDesigner.Types
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Layout
import IHP.IDE.CodeGen.Types
import qualified Data.Text as Text
import IHP.IDE.SchemaDesigner.View.Layout

data GeneratorsView = GeneratorsView

instance View GeneratorsView where
    html GeneratorsView = [hsx|
        <div class="generators">
            <div class="container pt-5">
                {renderFlashMessages}
                <div class="generators-list">
                    <a href={NewControllerAction} class="generator">
                        <div class="generator-icon">{copyIcon}</div>
                        <div class="generator-name">Controller</div>
                    </a>

                    <a href={NewActionAction} class="generator">
                        <div class="generator-icon">{copyIcon}</div>
                        <div class="generator-name">Action</div>
                    </a>
                    
                    <a href={NewViewAction} class="generator">
                        <div class="generator-icon">{copyIcon}</div>
                        <div class="generator-name">View</div>
                    </a>

                    <a href={NewMailAction} class="generator">
                        <div class="generator-icon">{copyIcon}</div>
                        <div class="generator-name">Mail</div>
                    </a>

                    <a href={NewScriptAction} class="generator">
                        <div class="generator-icon">{copyIcon}</div>
                        <div class="generator-name">Script</div>
                    </a>

                    <a href={NewMigrationAction} class="generator">
                        <div class="generator-icon">{databaseIcon}</div>
                        <div class="generator-name">Migration</div>
                    </a>

                    <a href={NewApplicationAction} class="generator">
                        <div class="generator-icon">{copyIcon}</div>
                        <div class="generator-name">Application</div>
                    </a>
                </div>
            </div>
        </div>
    |]

renderPlan (Left error) = [hsx|{error}|]
renderPlan (Right actions) = [hsx|<div class="generator-actions">{forEach actions renderGeneratorAction}</div>|]

renderGeneratorAction CreateFile { .. } = [hsx|
    <div class="generator-action CreateFile">
        <div class="file-path">{filePath}</div>
        <div class="file-content">{Text.strip fileContent}</div>
    </div>
|]
renderGeneratorAction AppendToFile { .. } = [hsx|
    <div class="generator-action AppendToFile">
        <div class="file-path">Append to {filePath}</div>
        <div class="file-content">{Text.strip fileContent}</div>
    </div>
|]
renderGeneratorAction AppendToMarker { .. } = [hsx|
    <div class="generator-action AppendToFile">
        <div class="file-path">Append to {filePath}</div>
        <div class="file-content">{Text.strip fileContent}</div>
    </div>
|]

renderGeneratorAction AddImport { .. } = [hsx|
    <div class="generator-action AddImport">
        <div class="file-path">Append to {filePath}</div>
        <div class="file-content">{Text.strip fileContent}</div>
    </div>
|]

renderGeneratorAction AddAction { .. } = [hsx|
    <div class="generator-action AddImport">
        <div class="file-path">Append to {filePath}</div>
        <div class="file-content">{Text.strip fileContent}</div>
    </div>
|]

renderGeneratorAction AddMountToFrontController { .. } = [hsx|
    <div class="generator-action AddMountToFrontController">
        <div class="file-path">Mount to FrontController {filePath}</div>
        <div class="file-content">{Text.strip applicationName}</div>
    </div>
|]

renderGeneratorAction AddToDataConstructor { .. } = [hsx|
    <div class="generator-action AddToDataConstructor">
        <div class="file-path">Append to {filePath}</div>
        <div class="file-content">{Text.strip fileContent}</div>
    </div>
|]

renderGeneratorAction EnsureDirectory {} = mempty
renderGeneratorAction RunShellCommand {} = mempty
