module IHP.IDE.CodeGen.View.Generators where

import IHP.ViewPrelude
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Layout
import IHP.IDE.CodeGen.Types
import qualified Data.Text as Text
import IHP.IDE.ToolServer.Helper.View

data GeneratorsView = GeneratorsView

instance View GeneratorsView where
    html GeneratorsView = [hsx|
        <div class="generators">
            <div class="container pt-5">
                {renderFlashMessages}
                <div class="generators-list">
                    {generator "Controller" (pathTo NewControllerAction) copyIcon}
                    {generator "Action" (pathTo NewActionAction) copyIcon}
                    {generator "View" (pathTo NewViewAction) copyIcon}
                    {generator "Mail" (pathTo NewMailAction) copyIcon}
                    {generator "Background Job" (pathTo NewJobAction) cogsIcon}
                    {generator "Script" (pathTo NewScriptAction) copyIcon}
                    {generator "Migration" (pathTo NewMigrationAction) dataIcon}
                    {generator "Application" (pathTo NewApplicationAction) copyIcon}
                </div>
            </div>
        </div>
    |]
        where
            generator :: Text -> Text -> Html -> Html
            generator name path icon = [hsx|
                <a href={path} class="generator">
                    <div class="generator-icon">{icon}</div>
                    <div class="generator-name">{name}</div>
                </a>
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
