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
            nameWithSuffix = if "Action" `isSuffixOf` name
                    then name
                    else name <> "Action" -- e.g. TestAction

            indexAction = Countable.pluralize singularName <> "Action"
            specialCases = [
                  (indexAction, indexContent)
                , ("Show" <> singularName <> "Action", showContent)
                , ("Edit" <> singularName <> "Action", editContent)
                , ("Update" <> singularName <> "Action", updateContent)
                , ("Create" <> singularName <> "Action", createContent)
                , ("Delete" <> singularName <> "Action", deleteContent)
                ]
            
            actionContent = 
                ""
                <> "    action " <> nameWithSuffix <> " = " <> "do" <> "\n"
                <> "        redirectTo "<> controllerName <> "Action\n"
            
            modelVariablePlural = lcfirst name
            modelVariableSingular = lcfirst singularName
            idFieldName = lcfirst singularName <> "Id"
            idType = "Id " <> singularName
            model = ucfirst singularName
            indexContent =
                ""
                <> "    action " <> name <> "Action = do\n"
                <> "        " <> modelVariablePlural <> " <- query @" <> model <> " |> fetch\n"
                <> "        render IndexView { .. }\n"

            newContent =
                ""
                <> "    action New" <> singularName <> "Action = do\n"
                <> "        let " <> modelVariableSingular <> " = newRecord\n"
                <> "        render NewView { .. }\n"

            showContent =
                ""
                <> "    action Show" <> singularName <> "Action { " <> idFieldName <> " } = do\n"
                <> "        " <> modelVariableSingular <> " <- fetch " <> idFieldName <> "\n"
                <> "        render ShowView { .. }\n"

            editContent =
                ""
                <> "    action Edit" <> singularName <> "Action { " <> idFieldName <> " } = do\n"
                <> "        " <> modelVariableSingular <> " <- fetch " <> idFieldName <> "\n"
                <> "        render EditView { .. }\n"

            updateContent =
                ""
                <> "    action Update" <> singularName <> "Action { " <> idFieldName <> " } = do\n"
                <> "        " <> modelVariableSingular <> " <- fetch " <> idFieldName <> "\n"
                <> "        " <> modelVariableSingular <> "\n" 
                <> "            |> build" <> singularName <> "\n"
                <> "            |> ifValid \\case\n"
                <> "                Left " <> modelVariableSingular <> " -> render EditView { .. }\n"
                <> "                Right " <> modelVariableSingular <> " -> do\n"
                <> "                    " <> modelVariableSingular <> " <- " <> modelVariableSingular <> " |> updateRecord\n"
                <> "                    setSuccessMessage \"" <> model <> " updated\"\n"
                <> "                    redirectTo Edit" <> singularName <> "Action { .. }\n"

            createContent =
                ""
                <> "    action Create" <> singularName <> "Action = do\n"
                <> "        let " <> modelVariableSingular <> " = newRecord @"  <> model <> "\n"
                <> "        " <> modelVariableSingular <> "\n" 
                <> "            |> build" <> singularName <> "\n"
                <> "            |> ifValid \\case\n"
                <> "                Left " <> modelVariableSingular <> " -> render NewView { .. } \n"
                <> "                Right " <> modelVariableSingular <> " -> do\n"
                <> "                    " <> modelVariableSingular <> " <- " <> modelVariableSingular <> " |> createRecord\n"
                <> "                    setSuccessMessage \"" <> model <> " created\"\n"
                <> "                    redirectTo " <> name <> "Action\n"

            deleteContent =
                ""
                <> "    action Delete" <> singularName <> "Action { " <> idFieldName <> " } = do\n"
                <> "        " <> modelVariableSingular <> " <- fetch " <> idFieldName <> "\n"
                <> "        deleteRecord " <> modelVariableSingular <> "\n"
                <> "        setSuccessMessage \"" <> model <> " deleted\"\n"
                <> "        redirectTo " <> name <> "Action\n"

            typesContentGeneric = 
                   "    | " <> nameWithSuffix

            typesContentWithParameter = 
                   "    | " <> nameWithSuffix <> " { " <> idFieldName <> " :: !(" <> idType <> ") }\n"
            

            chosenContent = fromMaybe actionContent (lookup nameWithSuffix specialCases)
            chosenType = if chosenContent `elem` [actionContent, newContent, createContent, indexContent]
                then typesContentGeneric
                else typesContentWithParameter

        in
            [ AddAction { filePath = get #applicationName config <> "/Controller/" <> controllerName <> ".hs", fileContent = chosenContent},
              AddToDataConstructor { dataConstructor = "data " <> controllerName, filePath = get #applicationName config <> "/Types.hs", fileContent = chosenType }
            ]
