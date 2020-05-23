module IHP.IDE.CodeGen.Controller where

import IHP.ControllerPrelude
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.ViewContext
import IHP.IDE.CodeGen.View.Generators
import IHP.IDE.CodeGen.View.NewController
import IHP.IDE.CodeGen.ControllerGenerator as ControllerGenerator

instance Controller CodeGenController where
    action GeneratorsAction = do
        render GeneratorsView

    action NewControllerAction = do
        let controllerName = paramOrDefault "" "name"
        plan <- ControllerGenerator.buildPlan controllerName
        render NewControllerView { .. }

    action CreateControllerAction = do
        let controllerName = param "name"
        ControllerGenerator.buildAndExecutePlan controllerName
        setSuccessMessage "Controller generated"
        redirectTo GeneratorsAction