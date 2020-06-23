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

buildPlan :: Text -> IO (Either Text [GeneratorAction])
buildPlan appAndControllerName = do
    schema <- SchemaDesigner.parseSchemaSql >>= \case
        Left parserError -> pure []
        Right statements -> pure statements

    pure case Text.splitOn "." appAndControllerName of
        [applicationName, controllerName'] -> do
            if isAlphaOnly applicationName
                then if isAlphaOnly controllerName'
                    then Right $ buildPlan' schema (ucfirst applicationName) controllerName'
                    else Left ("Invalid controller name: " <> tshow controllerName')
                else Left ("Invalid application name: " <> tshow applicationName)
        [controllerName'] -> if isAlphaOnly controllerName'
                then Right $ buildPlan' schema "Web" controllerName'
                else Left ("Invalid controller name: " <> tshow controllerName')
        _ -> Left "Name should be either 'ControllerName' or 'ApplicationName.ControllerName'"



buildPlan' schema applicationName controllerName' =
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
        <> generateViews schema config

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

generateViews :: [Statement] -> ControllerConfig -> [GeneratorAction]
generateViews schema config =
        let
            name = config |> get #controllerName
            singularName = config |> get #modelName --Post
            singularVariableName = lcfirst singularName --post
            pluralVariableName = lcfirst name --posts

            viewHeader moduleName =
                ""
                <> "module " <> qualifiedViewModuleName config moduleName <> " where\n"
                <> "import " <> get #applicationName config <> ".View.Prelude\n"
                <> "\n"


            indexAction = Countable.pluralize singularName <> "Action"

            modelFields :: [Text]
            modelFields = fieldsForTable schema pluralVariableName

            showView = 
                viewHeader "Show"
                <> "data ShowView = ShowView { " <> singularVariableName <> " :: " <> singularName <> " }\n"
                <> "\n"
                <> "instance View ShowView ViewContext where\n"
                <> "    html ShowView { .. } = [hsx|\n"
                <> "        <nav>\n"
                <> "            <ol class=\"breadcrumb\">\n"
                <> "                <li class=\"breadcrumb-item\"><a href={" <> indexAction <> "}>" <> Countable.pluralize name <> "</a></li>\n"
                <> "                <li class=\"breadcrumb-item active\">Show " <> singularName <> "</li>\n"
                <> "            </ol>\n"
                <> "        </nav>\n"
                <> "        <h1>Show " <> singularName <> "</h1>\n"
                <> "    |]\n"

            newView = 
                viewHeader "New"
                <> "data NewView = NewView { " <> singularVariableName <> " :: " <> singularName <> " }\n"
                <> "\n"
                <> "instance View NewView ViewContext where\n"
                <> "    html NewView { .. } = [hsx|\n"
                <> "        <nav>\n"
                <> "            <ol class=\"breadcrumb\">\n"
                <> "                <li class=\"breadcrumb-item\"><a href={" <> indexAction <> "}>" <> Countable.pluralize name <> "</a></li>\n"
                <> "                <li class=\"breadcrumb-item active\">Edit " <> singularName <> "</li>\n"
                <> "            </ol>\n"
                <> "        </nav>\n"
                <> "        <h1>New " <> singularName <> "</h1>\n"
                <> "        {renderForm " <> singularVariableName <> "}\n"
                <> "    |]\n"
                <> "\n"
                <> "renderForm :: " <> singularName <> " -> Html\n"
                <> "renderForm " <> singularVariableName <> " = formFor " <> singularVariableName <> " [hsx|\n"
                <> (intercalate "\n" (map (\field -> "    {textField #" <> field <> "}") modelFields)) <> "\n"
                <> "    {submitButton}\n"
                <> "|]\n"

            editView = 
                viewHeader "Edit"
                <> "data EditView = EditView { " <> singularVariableName <> " :: " <> singularName <> " }\n"
                <> "\n"
                <> "instance View EditView ViewContext where\n"
                <> "    html EditView { .. } = [hsx|\n"
                <> "        <nav>\n"
                <> "            <ol class=\"breadcrumb\">\n"
                <> "                <li class=\"breadcrumb-item\"><a href={" <> indexAction <> "}>" <> Countable.pluralize name <> "</a></li>\n"
                <> "                <li class=\"breadcrumb-item active\">Edit " <> singularName <> "</li>\n"
                <> "            </ol>\n"
                <> "        </nav>\n"
                <> "        <h1>Edit " <> singularName <> "</h1>\n"
                <> "        {renderForm " <> singularVariableName <> "}\n"
                <> "    |]\n"
                <> "\n"
                <> "renderForm :: " <> singularName <> " -> Html\n"
                <> "renderForm " <> singularVariableName <> " = formFor " <> singularVariableName <> " [hsx|\n"
                <> (intercalate "\n" (map (\field -> "    {textField #" <> field <> "}") modelFields)) <> "\n"
                <> "    {submitButton}\n"
                <> "|]\n"

            indexView = 
                viewHeader "Index"
                <> "data IndexView = IndexView { " <> pluralVariableName <> " :: [" <> singularName <> "] }\n"
                <> "\n"
                <> "instance View IndexView ViewContext where\n"
                <> "    html IndexView { .. } = [hsx|\n"
                <> "        <nav>\n"
                <> "            <ol class=\"breadcrumb\">\n"
                <> "                <li class=\"breadcrumb-item active\"><a href={" <> indexAction <> "}>" <> Countable.pluralize name <> "</a></li>\n"
                <> "            </ol>\n"
                <> "        </nav>\n"
                <> "        <h1>" <> name <> " <a href={pathTo New" <> singularName <> "Action} class=\"btn btn-primary ml-4\">+ New</a></h1>\n"
                <> "        <table class=\"table table-responsive\">\n"
                <> "            <thead>\n"
                <> "                <tr>\n"
                <> "                    <th>" <> singularName <> "</th>\n"
                <> "                    <th></th>\n"
                <> "                </tr>\n"
                <> "            </thead>\n"
                <> "            <tbody>{forEach " <> pluralVariableName <> " render" <> singularName <> "}</tbody>\n"
                <> "        </table>\n"
                <> "    |]\n"
                <> "\n\n"
                <> "render" <> singularName <> " " <> singularVariableName <> " = [hsx|\n"
                <> "    <tr>\n"
                <> "        <td>{" <> singularVariableName <> "}</td>\n"
                <> "        <td><a href={Show" <> singularName <> "Action (get #id " <> singularVariableName <> ")}>Show</a></td>\n"
                <> "        <td><a href={Edit" <> singularName <> "Action (get #id " <> singularVariableName <> ")} class=\"text-muted\">Edit</a></td>\n"
                <> "        <td><a href={Delete" <> singularName <> "Action (get #id " <> singularVariableName <> ")} class=\"js-delete text-muted\">Delete</a></td>\n"
                <> "    </tr>\n"
                <> "|]\n"
        in
            [ EnsureDirectory { directory = get #applicationName config <> "/View/" <> name }
            , CreateFile { filePath = get #applicationName config <> "/View/" <> name <> "/Show.hs", fileContent = showView }
            , CreateFile { filePath = get #applicationName config <> "/View/" <> name <> "/New.hs", fileContent = newView }
            , CreateFile { filePath = get #applicationName config <> "/View/" <> name <> "/Edit.hs", fileContent = editView }
            , CreateFile { filePath = get #applicationName config <> "/View/" <> name <> "/Index.hs", fileContent = indexView }
            ]



isAlphaOnly :: Text -> Bool
isAlphaOnly text = Text.all (\c -> Char.isAlpha c || c == '_') text

