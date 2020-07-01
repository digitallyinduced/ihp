module IHP.IDE.CodeGen.ActionGenerator (buildPlan) where

import IHP.Prelude
import IHP.HaskellSupport
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import IHP.ViewSupport
import qualified System.Process as Process
import IHP.IDE.CodeGen.Types
import qualified IHP.IDE.SchemaDesigner.Parser as SchemaDesigner
import IHP.IDE.SchemaDesigner.Types
import qualified Text.Countable as Countable

data ActionConfig = ActionConfig
    { controllerName :: Text 
    , applicationName :: Text
    , modelName :: Text
    , actionName :: Text
    } deriving (Eq, Show)

buildPlan :: Text -> Text -> Text -> IO (Either Text [GeneratorAction])
buildPlan actionName applicationName controllerName =
    if (null actionName || null controllerName)
        then pure $ Left "Action name and controller name cannot be empty"
        else do 
            schema <- SchemaDesigner.parseSchemaSql >>= \case
                Left parserError -> pure []
                Right statements -> pure statements
            let modelName = tableNameToModelName controllerName
            let actionConfig = ActionConfig {controllerName, applicationName, modelName, actionName }
            pure $ Right $ generateGenericAction schema actionConfig

-- E.g. qualifiedViewModuleName config "Edit" == "Web.View.Users.Edit"
--qualifiedViewModuleName :: ActionConfig -> Text -> Text
--qualifiedViewModuleName config viewName =
--    get #applicationName config <> ".View." <> get #controllerName config <> "." <> viewName

generateGenericAction :: [Statement] -> ActionConfig -> [GeneratorAction]
generateGenericAction schema config = 
        let 
            controllerName = get #controllerName config
            name = get #actionName config
            singularName = config |> get #modelName
            singularVariableName = lcfirst singularName
            pluralVariableName = lcfirst controllerName
            nameWithSuffix = if ("Action" `isSuffixOf` name) then name else name <> "Action" -- e.g. TestAction

            indexAction = Countable.pluralize singularName <> "Action"
            
            actionContent = 
                "    action " <> nameWithSuffix <> " = " <> "do" <> "\n"
                <> "        redirectTo "<> controllerName <> "Action\n"

            typesContent = "    | " <> nameWithSuffix
        in
            [ AddAction { filePath = get #applicationName config <> "/Controller/" <> controllerName <> ".hs", fileContent = actionContent},
              AddToDataConstructor { dataConstructor = "data " <> controllerName, filePath = get #applicationName config <> "/Types.hs", fileContent = typesContent }
            ]