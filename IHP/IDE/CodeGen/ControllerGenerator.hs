module IHP.IDE.CodeGen.ControllerGenerator (buildPlan) where

import ClassyPrelude
import IHP.NameSupport
import Data.String.Conversions (cs)
import Data.Text.IO (appendFile)
import qualified System.Exit as Exit
import IHP.HaskellSupport
import qualified Data.Text as Text
import qualified Text.Countable as Countable
import qualified Data.Char as Char
import qualified IHP.IDE.SchemaDesigner.Parser as SchemaDesigner
import IHP.IDE.SchemaDesigner.Types
import qualified System.Posix.Env.ByteString as Posix
import Control.Monad.Fail
import IHP.IDE.CodeGen.Types
import qualified IHP.IDE.CodeGen.ViewGenerator as ViewGenerator


buildPlan :: Text -> Text -> IO (Either Text [GeneratorAction])
buildPlan rawControllerName applicationName = do
    schema <- SchemaDesigner.parseSchemaSql >>= \case
        Left parserError -> pure []
        Right statements -> pure statements
    let controllerName = tableNameToControllerName rawControllerName
    let modelName = tableNameToModelName rawControllerName
    viewPlans <- generateViews applicationName controllerName
    pure $ Right $ buildPlan' schema applicationName controllerName modelName viewPlans

buildPlan' schema applicationName controllerName modelName viewPlans =
    let
        config = ControllerConfig { modelName, controllerName, applicationName }
    in
        [ CreateFile { filePath = applicationName <> "/Controller/" <> controllerName <> ".hs", fileContent = (generateController schema config) }
        , AppendToFile { filePath = applicationName <> "/Routes.hs", fileContent = (controllerInstance config) }
        , AppendToFile { filePath = applicationName <> "/Types.hs", fileContent = (generateControllerData config) }
        , AppendToMarker { marker = "-- Controller Imports", filePath = applicationName <> "/FrontController.hs", fileContent = ("import " <> applicationName <> ".Controller." <> controllerName) }
        , AppendToMarker { marker = "-- Generator Marker", filePath = applicationName <> "/FrontController.hs", fileContent = ("        , parseRoute @" <> controllerName <> "Controller") }
        ]
        <> viewPlans

data ControllerConfig = ControllerConfig
    { controllerName :: Text 
    , applicationName :: Text
    , modelName :: Text
    } deriving (Eq, Show)

controllerInstance :: ControllerConfig -> Text
controllerInstance ControllerConfig { controllerName, modelName, applicationName } =
    "instance AutoRoute " <> controllerName <> "Controller\n"
    <> "type instance ModelControllerMap " <> applicationName <> "Application " <> modelName <> " = " <> controllerName <> "Controller\n\n"

data HaskellModule = HaskellModule { moduleName :: Text, body :: Text }

getTable :: [Statement] -> Text -> Maybe Statement
getTable schema name = find isTable schema
    where
        isTable :: Statement -> Bool
        isTable table@(CreateTable { name = name' }) | name == name' = True
        isTable _ = False

fieldsForTable :: [Statement] -> Text -> [Text]
fieldsForTable database name =
    case getTable database name of
        Just (CreateTable { columns }) -> columns
                |> filter (\col -> isNothing (get #defaultValue col))
                |> map (get #name)
                |> map columnNameToFieldName
        _ -> []


generateControllerData :: ControllerConfig -> Text
generateControllerData config =
    let
        pluralName = Countable.pluralize $ get #controllerName config
        name = get #controllerName config
        singularName = get #modelName config
        idFieldName = lcfirst singularName <> "Id"
        idType = "Id " <> singularName
    in 
        "\n"
        <> "data " <> name <> "Controller\n"
        <> "    = " <> pluralName <> "Action\n"
        <> "    | New" <> singularName <> "Action\n"
        <> "    | Show" <> singularName <> "Action { " <> idFieldName <> " :: !(" <> idType <> ") }\n"
        <> "    | Create" <> singularName <> "Action\n"
        <> "    | Edit" <> singularName <> "Action { " <> idFieldName <> " :: !(" <> idType <> ") }\n"
        <> "    | Update" <> singularName <> "Action { " <> idFieldName <> " :: !(" <> idType <> ") }\n"
        <> "    | Delete" <> singularName <> "Action { " <> idFieldName <> " :: !(" <> idType <> ") }\n"
        <> "    deriving (Eq, Show, Data)\n"

generateController :: [Statement] -> ControllerConfig -> Text
generateController schema config =
    let
        applicationName = get #applicationName config
        name = config |> get #controllerName
        pluralName = Countable.pluralize $ get #controllerName config
        singularName = config |> get #modelName
        moduleName =  applicationName <> ".Controller." <> name
        controllerName = name <> "Controller"

        importStatements =
            [ "import " <> applicationName <> ".Controller.Prelude"
            , "import " <> qualifiedViewModuleName config "Index"
            , "import " <> qualifiedViewModuleName config "New"
            , "import " <> qualifiedViewModuleName config "Edit"
            , "import " <> qualifiedViewModuleName config "Show"

            ]

        modelVariablePlural = lcfirst name
        modelVariableSingular = lcfirst singularName
        idFieldName = lcfirst singularName <> "Id"
        model = ucfirst singularName
        indexAction =
            ""
            <> "    action " <> pluralName <> "Action = do\n"
            <> "        " <> modelVariablePlural <> " <- query @" <> model <> " |> fetch\n"
            <> "        render IndexView { .. }\n"

        newAction =
            ""
            <> "    action New" <> singularName <> "Action = do\n"
            <> "        let " <> modelVariableSingular <> " = newRecord\n"
            <> "        render NewView { .. }\n"

        showAction =
            ""
            <> "    action Show" <> singularName <> "Action { " <> idFieldName <> " } = do\n"
            <> "        " <> modelVariableSingular <> " <- fetch " <> idFieldName <> "\n"
            <> "        render ShowView { .. }\n"

        editAction =
            ""
            <> "    action Edit" <> singularName <> "Action { " <> idFieldName <> " } = do\n"
            <> "        " <> modelVariableSingular <> " <- fetch " <> idFieldName <> "\n"
            <> "        render EditView { .. }\n"

        modelFields :: [Text]
        modelFields = fieldsForTable schema (modelNameToTableName modelVariableSingular)

        updateAction =
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

        createAction =
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
            <> "                    redirectTo " <> pluralName <> "Action\n"

        deleteAction =
            ""
            <> "    action Delete" <> singularName <> "Action { " <> idFieldName <> " } = do\n"
            <> "        " <> modelVariableSingular <> " <- fetch " <> idFieldName <> "\n"
            <> "        deleteRecord " <> modelVariableSingular <> "\n"
            <> "        setSuccessMessage \"" <> model <> " deleted\"\n"
            <> "        redirectTo " <> pluralName <> "Action\n"

        fromParams =
            ""
            <> "build" <> singularName <> " " <> modelVariableSingular <> " = " <> modelVariableSingular <> "\n"
            <> "    |> fill " <> toTypeLevelList modelFields <> "\n"

        toTypeLevelList values = "@" <> (if length values < 2 then "'" else "") <> tshow values
    in
        ""
        <> "module " <> moduleName <> " where" <> "\n"
        <> "\n"
        <> intercalate "\n" importStatements
        <> "\n\n"
        <> "instance Controller " <> controllerName <> " where\n"
        <> indexAction
        <> "\n"
        <> newAction
        <> "\n"
        <> showAction
        <> "\n"
        <> editAction
        <> "\n"
        <> updateAction
        <> "\n"
        <> createAction
        <> "\n"
        <> deleteAction
        <> "\n"
        <> fromParams

-- E.g. qualifiedViewModuleName config "Edit" == "Web.View.Users.Edit"
qualifiedViewModuleName :: ControllerConfig -> Text -> Text
qualifiedViewModuleName config viewName =
    get #applicationName config <> ".View." <> get #controllerName config <> "." <> viewName

pathToModuleName :: Text -> Text
pathToModuleName moduleName = Text.replace "." "/" moduleName

generateViews :: Text -> Text -> IO [GeneratorAction]
generateViews applicationName controllerName = 
    if null controllerName 
        then pure []
        else do
            (Right indexPlan) <- ViewGenerator.buildPlan "IndexView" applicationName controllerName
            (Right newPlan) <- ViewGenerator.buildPlan "NewView" applicationName controllerName
            (Right showPlan) <- ViewGenerator.buildPlan "ShowView" applicationName controllerName
            (Right editPlan) <- ViewGenerator.buildPlan "EditView" applicationName controllerName
            pure $ indexPlan <> newPlan <> showPlan <> editPlan


isAlphaOnly :: Text -> Bool
isAlphaOnly text = Text.all (\c -> Char.isAlpha c || c == '_') text

