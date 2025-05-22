module IHP.IDE.CodeGen.ActionGenerator (buildPlan) where

import IHP.Prelude
import qualified Data.Text as Text
import IHP.IDE.CodeGen.Types
import qualified IHP.IDE.SchemaDesigner.Parser as SchemaDesigner
import IHP.IDE.SchemaDesigner.Types
import qualified IHP.IDE.CodeGen.ViewGenerator as ViewGenerator

data ActionConfig = ActionConfig
    { controllerName :: Text
    , applicationName :: Text
    , modelName :: Text
    , actionName :: Text
    } deriving (Eq, Show)

buildPlan :: Text -> Text -> Text -> Bool -> IO (Either Text [GeneratorAction])
buildPlan actionName applicationName controllerName doGenerateView=
    if (null actionName || null controllerName)
        then pure $ Left "Neither action name nor controller name can be empty"
        else do
            schema <- SchemaDesigner.parseSchemaSql >>= \case
                Left parserError -> pure []
                Right statements -> pure statements
            let actionConfig = ActionConfig {controllerName, applicationName, modelName, actionName }
            let actionPlan = generateGenericAction schema actionConfig doGenerateView
            if doGenerateView
                then do
                    viewPlan <- ViewGenerator.buildPlan viewName applicationName controllerName
                    case viewPlan of
                        Right viewPlan' -> pure $ Right $ actionPlan ++ viewPlan'
                        Left error -> pure $ Left error
                else pure $ Right $ actionPlan
    where
        viewName = ucfirst $ if "Action" `isSuffixOf` actionName
                             then Text.dropEnd 6 actionName
                             else actionName
        modelName = tableNameToModelName controllerName

-- E.g. qualifiedViewModuleName config "Edit" == "Web.View.Users.Edit"
-- qualifiedViewModuleName :: ActionConfig -> Text -> Text
-- qualifiedViewModuleName config viewName =
--    config.applicationName <> ".View." <> config.controllerName <> "." <> viewName

generateGenericAction :: [Statement] -> ActionConfig -> Bool -> [GeneratorAction]
generateGenericAction schema config doGenerateView =
        let
            controllerName = config.controllerName
            name = ucfirst $ config.actionName
            singularName = config.modelName
            nameWithSuffix = if "Action" `isSuffixOf` name
                    then name
                    else name <> "Action" -- e.g. TestAction
            viewName = if "Action" `isSuffixOf` name
                then Text.dropEnd 6 name
                else name
            indexAction = pluralize singularName <> "Action"
            specialCases = [
                  (indexAction, indexContent)
                , ("Show" <> singularName <> "Action", showContent)
                , ("Edit" <> singularName <> "Action", editContent)
                , ("Update" <> singularName <> "Action", updateContent)
                , ("Create" <> singularName <> "Action", createContent)
                , ("Delete" <> singularName <> "Action", deleteContent)
                ]

            actionContent = if doGenerateView
                then
                        "    action " <> nameWithSuffix <> " = " <> "do" <> "\n"
                    <>  "        render " <> viewName <> "View { .. }\n"
                else
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
                <> "                Left " <> modelVariableSingular <> " -> render NewView { .. }\n"
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
            [ AddAction { filePath = config.applicationName <> "/Controller/" <> controllerName <> ".hs", fileContent = chosenContent}
            , AddToDataConstructor { dataConstructor = "data " <> controllerName, filePath = config.applicationName <> "/Types.hs", fileContent = chosenType }
            ]
