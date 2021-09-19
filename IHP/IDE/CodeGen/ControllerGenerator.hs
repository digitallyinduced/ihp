module IHP.IDE.CodeGen.ControllerGenerator (buildPlan, buildPlan') where

import ClassyPrelude
import IHP.NameSupport
import IHP.HaskellSupport
import qualified Data.Text as Text
import qualified Data.Char as Char
import qualified IHP.IDE.SchemaDesigner.Parser as SchemaDesigner
import IHP.IDE.SchemaDesigner.Types
import IHP.IDE.CodeGen.Types
import qualified IHP.IDE.CodeGen.ViewGenerator as ViewGenerator


buildPlan :: Text -> Text -> Bool -> IO (Either Text [GeneratorAction])
buildPlan rawControllerName applicationName paginationEnabled = do
    schema <- SchemaDesigner.parseSchemaSql >>= \case
        Left parserError -> pure []
        Right statements -> pure statements
    let controllerName = tableNameToControllerName rawControllerName
    let modelName = tableNameToModelName rawControllerName
    pure $ Right $ buildPlan' schema applicationName controllerName modelName paginationEnabled

buildPlan' schema applicationName controllerName modelName paginationEnabled =
    let
        config = ControllerConfig { modelName, controllerName, applicationName, paginationEnabled }
        viewPlans = generateViews schema applicationName controllerName paginationEnabled
    in
        [ CreateFile { filePath = applicationName <> "/Controller/" <> controllerName <> ".hs", fileContent = (generateController schema config) }
        , AppendToFile { filePath = applicationName <> "/Routes.hs", fileContent = "\n" <> (controllerInstance config) }
        , AppendToFile { filePath = applicationName <> "/Types.hs", fileContent = (generateControllerData config) }
        , AppendToMarker { marker = "-- Controller Imports", filePath = applicationName <> "/FrontController.hs", fileContent = ("import " <> applicationName <> ".Controller." <> controllerName) }
        , AppendToMarker { marker = "-- Generator Marker", filePath = applicationName <> "/FrontController.hs", fileContent = ("        , parseRoute @" <> controllerName <> "Controller") }
        ]
        <> viewPlans

data ControllerConfig = ControllerConfig
    { controllerName :: Text
    , applicationName :: Text
    , modelName :: Text
    , paginationEnabled :: Bool
    } deriving (Eq, Show)

controllerInstance :: ControllerConfig -> Text
controllerInstance ControllerConfig { controllerName, modelName, applicationName } =
    "instance AutoRoute " <> controllerName <> "Controller\n\n"

data HaskellModule = HaskellModule { moduleName :: Text, body :: Text }

generateControllerData :: ControllerConfig -> Text
generateControllerData config =
    let
        name = get #controllerName config
        pluralName = get #controllerName config |> lcfirst |> pluralize |> ucfirst
        singularName = get #modelName config |> lcfirst |> singularize |> ucfirst
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
        pluralName = get #controllerName config |> lcfirst |> pluralize |> ucfirst
        singularName = get #modelName config |> lcfirst |> singularize |> ucfirst
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
        paginationEnabled = get #paginationEnabled config

        indexAction =
            ""
            <> "    action " <> pluralName <> "Action = do\n"
            <> (if paginationEnabled
                then   "        (" <> modelVariablePlural <> "Q, pagination) <- query @" <> model <> " |> paginate\n"
                    <> "        " <> modelVariablePlural <> " <- " <> modelVariablePlural <> "Q |> fetch\n"
                else "        " <> modelVariablePlural <> " <- query @" <> model <> " |> fetch\n"
            )
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
        modelFields = [ modelNameToTableName modelVariableSingular, modelVariableSingular ]
                |> mapMaybe (fieldsForTable schema)
                |> headMay
                |> fromMaybe []

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

generateViews :: [Statement] -> Text -> Text -> Bool -> [GeneratorAction]
generateViews schema applicationName controllerName' paginationEnabled =
    if null controllerName'
        then []
        else do
            let indexPlan = ViewGenerator.buildPlan' schema (config "IndexView")
            let newPlan = ViewGenerator.buildPlan' schema (config "NewView")
            let showPlan = ViewGenerator.buildPlan' schema (config "ShowView")
            let editPlan = ViewGenerator.buildPlan' schema (config "EditView")
            indexPlan <> newPlan <> showPlan <> editPlan
    where
        config viewName = do
            let modelName = tableNameToModelName controllerName'
            let controllerName = tableNameToControllerName controllerName'
            ViewGenerator.ViewConfig { .. }


isAlphaOnly :: Text -> Bool
isAlphaOnly text = Text.all (\c -> Char.isAlpha c || c == '_') text
