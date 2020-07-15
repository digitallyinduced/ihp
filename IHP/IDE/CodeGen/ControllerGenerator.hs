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

normalizeControllerName :: Text -> Either Text [Text]
normalizeControllerName appAndControllerName = do
    case Text.splitOn "." appAndControllerName of
        [applicationName, controllerName'] -> do
            if isAlphaOnly applicationName
                then if isAlphaOnly controllerName'
                    then Right [applicationName, controllerName']
                    else Left ("Invalid controller name: " <> tshow controllerName')
                else Left ("Invalid application name: " <> tshow applicationName)
        [controllerName'] -> if isAlphaOnly controllerName'
                then Right $ [controllerName']
                else Left ("Invalid controller name: " <> tshow controllerName')
        _ -> Left "Name should be either 'ControllerName' or 'ApplicationName.ControllerName'"

buildPlan :: Text -> IO (Either Text [GeneratorAction])
buildPlan appAndControllerName = do
    schema <- SchemaDesigner.parseSchemaSql >>= \case
        Left parserError -> pure []
        Right statements -> pure statements
    viewPlans <- generateViews applicationName controllerName
    pure $ Right $ buildPlan' schema applicationName controllerName viewPlans

buildPlan' schema applicationName controllerName' viewPlans =
    let
        modelName = tableNameToModelName controllerName'
        controllerName = Countable.pluralize modelName
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
        Nothing -> []


generateControllerData :: ControllerConfig -> Text
generateControllerData config =
    let
        name = get #controllerName config
        singularName = get #modelName config
        idFieldName = lcfirst singularName <> "Id"
        idType = "Id " <> singularName
    in 
        "\n"
        <> "data " <> name <> "Controller\n"
        <> "    = " <> name <> "Action\n"
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
            <> "    action " <> name <> "Action = do\n"
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
        modelFields = fieldsForTable schema modelVariablePlural

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
            <> "                    redirectTo " <> name <> "Action\n"

        deleteAction =
            ""
            <> "    action Delete" <> singularName <> "Action { " <> idFieldName <> " } = do\n"
            <> "        " <> modelVariableSingular <> " <- fetch " <> idFieldName <> "\n"
            <> "        deleteRecord " <> modelVariableSingular <> "\n"
            <> "        setSuccessMessage \"" <> model <> " deleted\"\n"
            <> "        redirectTo " <> name <> "Action\n"

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
    get #applicationName config <> ".View." <> Countable.pluralize (get #controllerName config) <> "." <> viewName

pathToModuleName :: Text -> Text
pathToModuleName moduleName = Text.replace "." "/" moduleName

generateViews :: Text -> Text -> IO [GeneratorAction]
generateViews applicationName controllerName = do
    when (null controllerName) $ pure []
    (Right indexPlan) <- ViewGenerator.buildPlan "IndexView" applicationName controllerName
    (Right newPlan) <- ViewGenerator.buildPlan "NewView" applicationName controllerName
    (Right showPlan) <- ViewGenerator.buildPlan "ShowView" applicationName controllerName
    (Right editPlan) <- ViewGenerator.buildPlan "EditView" applicationName controllerName
    pure $ indexPlan <> newPlan <> showPlan <> editPlan


isAlphaOnly :: Text -> Bool
isAlphaOnly text = Text.all (\c -> Char.isAlpha c || c == '_') text

