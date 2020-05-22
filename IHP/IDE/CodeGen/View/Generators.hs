module IHP.IDE.CodeGen.View.Generators where

import IHP.ViewPrelude
import IHP.IDE.SchemaDesigner.Types
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Layout
import IHP.View.Modal
import IHP.IDE.SchemaDesigner.View.Layout

data GeneratorsView = GeneratorsView

instance View GeneratorsView ViewContext where
    html GeneratorsView = [hsx|
        <div class="generators">
            <div class="container pt-5">
                <div class="generators-list">
                    <a href={NewControllerAction} class="generator">
                        <div class="generator-icon">{copyIcon}</div>
                        <div class="generator-name">Controller</div>
                    </a>

                    <div class="generator">
                        <div class="generator-icon">{copyIcon}</div>
                        <div class="generator-name">Action</div>
                    </div>
                    
                    <div class="generator">
                        <div class="generator-icon">{copyIcon}</div>
                        <div class="generator-name">View</div>
                    </div>

                    <div class="generator">
                        <div class="generator-icon">{copyIcon}</div>
                        <div class="generator-name">Script</div>
                    </div>

                    <div class="generator">
                        <div class="generator-icon">{copyIcon}</div>
                        <div class="generator-name">Application</div>
                    </div>
                </div>
            </div>
        </div>
    |]