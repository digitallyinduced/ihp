module IHP.IDE.CodeGen.View.Generators where

import IHP.ViewPrelude
import IHP.IDE.SchemaDesigner.Types
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Layout
import IHP.View.Modal
import IHP.IDE.CodeGen.Types
import qualified Data.Text as Text
import IHP.IDE.SchemaDesigner.View.Layout

data GeneratorsView = GeneratorsView

instance View GeneratorsView ViewContext where
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

                    <a href={NewScriptAction} class="generator">
                        <div class="generator-icon">{copyIcon}</div>
                        <div class="generator-name">Script</div>
                    </a>

                    <div class="generator">
                        <div class="generator-icon">{copyIcon}</div>
                        <div class="generator-name">Application</div>
                    </div>
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
        <div class="file-content">import {Text.strip fileContent}</div>
    </div>
|]

renderGeneratorAction AddAction { .. } = [hsx|
    <div class="generator-action AddImport">
        <div class="file-path">Append to {filePath}</div>
        <div class="file-content">{Text.strip fileContent}</div>
    </div>
|]

renderGeneratorAction EnsureDirectory {} = mempty
renderGeneratorAction RunShellCommand {} = mempty

renderGeneratorAction action = [hsx|{action}|]