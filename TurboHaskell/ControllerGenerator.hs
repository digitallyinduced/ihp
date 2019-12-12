{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module TurboHaskell.ControllerGenerator where

import ClassyPrelude
import TurboHaskell.NameSupport
import TurboHaskell.SchemaSupport
import Data.String.Conversions (cs)
import Data.Text.IO (appendFile)
import qualified Data.Text as Text
import qualified System.Directory as Directory
import qualified System.Exit as Exit
import TurboHaskell.SchemaTypes
import TurboHaskell.HaskellSupport
import qualified Data.Text as Text
import qualified Text.Countable as Countable
import qualified Data.Char as Char

main' :: [Table] -> [Text] -> IO ()
main' database args = do
    case headMay args of
        Just "" -> usage
        Just appAndControllerName -> do
            case Text.splitOn "." appAndControllerName of
                [applicationName, controllerName'] -> do
                    if isAlphaOnly applicationName
                        then if isAlphaOnly controllerName'
                            then gen database (ucfirst applicationName) controllerName'
                            else putStrLn ("Invalid controller name: " <> tshow controllerName')
                        else putStrLn ("Invalid application name: " <> tshow applicationName)
                [controllerName'] -> if isAlphaOnly controllerName'
                        then gen database "Web" controllerName'
                        else putStrLn ("Invalid controller name: " <> tshow controllerName')
                [] -> usage
        Nothing -> usage


isAlphaOnly :: Text -> Bool
isAlphaOnly text = Text.all (\c -> Char.isAlpha c || c == '_') text

gen database applicationName controllerName' = do
    let modelName = tableNameToModelName controllerName'
    let controllerName = Countable.pluralize modelName
    let config = ControllerConfig { modelName, controllerName, applicationName }
    let generate =
            [ CreateFile { filePath = applicationName <> "/Controller/" <> controllerName <> ".hs", fileContent = (generateController database config) }
            , AppendToFile { filePath = applicationName <> "/Routes.hs", fileContent = (controllerInstance config) }
            , AppendToFile { filePath = applicationName <> "/Types.hs", fileContent = (generateControllerData config) }
            , AppendToMarker { marker = "-- Controller Imports", filePath = applicationName <> "/FrontController.hs", fileContent = ("import " <> applicationName <> ".Controller." <> controllerName) }
            , AppendToMarker { marker = "-- Generator Marker", filePath = applicationName <> "/FrontController.hs", fileContent = ("        , parseRoute @" <> controllerName <> "Controller") }
            ]
            <> generateViews database config
    evalActions generate

data ControllerConfig = ControllerConfig
    { controllerName :: Text 
    , applicationName :: Text
    , modelName :: Text
    } deriving (Eq, Show, Generic)

usage :: IO ()
usage = putStrLn "Usage: new-controller RESOURCE_NAME"

controllerInstance :: ControllerConfig -> Text
controllerInstance ControllerConfig { controllerName, modelName, applicationName } =
    "instance RestfulController " <> controllerName <> "Controller\n"
    <> "type instance ModelControllerMap " <> applicationName <> " " <> modelName <> " = " <> controllerName <> "Controller\n"
    <> "type instance ControllerApplicationMap " <> controllerName <> "Controller" <> " = " <> applicationName <> "\n\n"

data GeneratorAction
    = CreateFile { filePath :: Text, fileContent :: Text }
    | AppendToFile { filePath :: Text, fileContent :: Text }
    | AppendToMarker { marker :: Text, filePath :: Text, fileContent :: Text }
    | EnsureDirectory { directory :: Text }
    deriving (Show, Eq)

data HaskellModule = HaskellModule { moduleName :: Text, body :: Text }

evalActions :: [GeneratorAction] -> IO ()
evalActions actions = forM_ actions evalAction'
    where
        evalAction' CreateFile { filePath, fileContent } = do
            putStrLn (">>>>>>>>>>>> CREATE " <> filePath)
            putStrLn fileContent
            putStrLn "\n\n"
        evalAction' AppendToFile { filePath, fileContent } = do
            putStrLn (">>>>>>>>>>>> APPEND " <> filePath)
            putStrLn fileContent
            putStrLn "\n\n"
        evalAction' AppendToMarker { marker, filePath, fileContent } = do
            putStrLn (">>>>>>>>>>>> APPEND " <> marker <> " => " <> filePath)
            putStrLn fileContent
            putStrLn "\n\n"
        evalAction' otherwise = do
            putStrLn $ ">>>>>>>>>>>>" <> tshow otherwise

        evalAction CreateFile { filePath, fileContent } = do
            writeFile (cs filePath) (cs fileContent)
            putStrLn ("+ " <> filePath)
        evalAction AppendToFile { filePath, fileContent } = do
            appendFile (cs filePath) fileContent
            putStrLn ("* " <> filePath)
        evalAction AppendToMarker { marker, filePath, fileContent } = do
            content <- readFile (cs filePath)
            let newContent = Text.replace marker (marker <> "\n" <> cs fileContent) (cs content)
            writeFile (cs filePath) (cs newContent)
            putStrLn ("* " <> filePath <> " (import)")
        evalAction EnsureDirectory { directory } = do
            Directory.createDirectoryIfMissing True (cs directory)

describePlan :: [GeneratorAction] -> Text
describePlan actions = intercalate "\n" (map describePlan' actions)

describePlan' :: GeneratorAction -> Text
describePlan' CreateFile { filePath, fileContent } = "CREATE " <> filePath
describePlan' AppendToFile { filePath, fileContent } = "APPEND " <> filePath <> ": " <> fileContent
describePlan' AppendToMarker { marker, filePath, fileContent } = "APPEND MARKER " <> marker <> " => " <> filePath <> ": " <> fileContent
describePlan' EnsureDirectory { directory } = "DIRECTORY " <> directory

getTable :: [Table] -> Text -> Maybe Table
getTable database name = find (\(Table n _) -> n == name) database

fieldsForTable :: [Table] -> Text -> [Text]
fieldsForTable database name =
    case getTable database name of
        Just (Table _ attributes) -> map (\(Field name _) -> columnNameToFieldName name) (fieldsWithoutDefaultValue $ fieldsOnly attributes)
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
        <> "    deriving (Eq, Show, Generic, Data)\n"

generateController :: [Table] -> ControllerConfig -> Text
generateController database config =
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
        modelFields = fieldsForTable database modelVariablePlural

        updateAction =
            ""
            <> "    action Update" <> singularName <> "Action { " <> idFieldName <> " } = do\n"
            <> "        " <> modelVariableSingular <> " <- fetch " <> idFieldName <> "\n"
            <> "        runPipeline " <> modelVariableSingular <> " build" <> singularName <> " >>= \\case\n"
            <> "            Left " <> modelVariableSingular <> " -> render EditView { .. }\n"
            <> "            Right " <> modelVariableSingular <> " -> do\n"
            <> "                " <> modelVariableSingular <> " <- " <> modelVariableSingular <> " |> updateRecord\n"
            <> "                setSuccessMessage \"" <> model <> " updated\"\n"
            <> "                redirectTo Edit" <> singularName <> "Action { .. }\n"

        createAction =
            ""
            <> "    action Create" <> singularName <> "Action = do\n"
            <> "        let " <> modelVariableSingular <> " = newRecord @New"  <> model <> "\n"
            <> "        runPipeline " <> modelVariableSingular <> " build" <> singularName <> " >>= \\case\n"
            <> "            Left " <> modelVariableSingular <> " -> render NewView { .. } \n"
            <> "            Right " <> modelVariableSingular <> " -> do\n"
            <> "                " <> modelVariableSingular <> " <- " <> modelVariableSingular <> " |> createRecord\n"
            <> "                setSuccessMessage \"" <> model <> " created\"\n"
            <> "                redirectTo " <> name <> "Action\n"

        deleteAction =
            ""
            <> "    action Delete" <> singularName <> "Action { " <> idFieldName <> " } = do\n"
            <> "        " <> modelVariableSingular <> " <- fetch " <> idFieldName <> "\n"
            <> "        deleteRecord " <> modelVariableSingular <> "\n"
            <> "        setSuccessMessage \"" <> model <> " deleted\"\n"
            <> "        redirectTo " <> name <> "Action\n"

        fromParams =
            ""
            <> "build" <> singularName <> " :: _ => RecordReader " <> modelVariableSingular <> "\n"
            <> "build" <> singularName <> " = fill " <> toTypeLevelList modelFields <> "\n"

        toTypeLevelList values = "@" <> (if length values < 2 then "'" else "") <> tshow values


        fromParamsInstanceHeadArgs = 
            case getTable database (lcfirst name) of
                Just (Table _ attributes) ->
                    attributes
                    |> fieldsOnly
                    |> fieldsWithDefaultValue
                    |> map (\(Field fieldName _) -> columnNameToFieldName fieldName)
                    |> Text.unwords
                Nothing -> ""
        fromParamsInstanceHead = "NewOrSaved" <> singularName <> " " <> fromParamsInstanceHeadArgs 
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

generateViews :: [Table] -> ControllerConfig -> [GeneratorAction]
generateViews database config =
        let
            name = config |> get #controllerName
            singularName = config |> get #modelName
            singularVariableName = lcfirst singularName
            pluralVariableName = lcfirst name

            viewHeader moduleName =
                ""
                <> "module " <> qualifiedViewModuleName config moduleName <> " where\n"
                <> "import " <> get #applicationName config <> ".View.Prelude\n"
                <> "\n"


            indexAction = Countable.pluralize singularName <> "Action"

            modelFields :: [Text]
            modelFields = fieldsForTable database pluralVariableName

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
                <> "data NewView = NewView { " <> singularVariableName <> " :: New" <> singularName <> " }\n"
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
                <> "renderForm :: New" <> singularName <> " -> Html\n"
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
                <> "            <tbody>{forM_ " <> pluralVariableName <> " render" <> singularName <> "}</tbody>\n"
                <> "        </table>\n"
                <> "    |]\n"
                <> "\n\n"
                <> "render" <> singularName <> " " <> singularVariableName <> " = [hsx|\n"
                <> "    <tr>\n"
                <> "        <td>{" <> singularVariableName <> "}</td>\n"
                <> "        <td><a href={Show" <> singularName <> "Action (get #id " <> singularVariableName <> ")}>Show</a></td>\n"
                <> "        <td><a href={Edit" <> singularName <> "Action (get #id " <> singularVariableName <> ")} class=\"text-muted\">edit</a></td>\n"
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
