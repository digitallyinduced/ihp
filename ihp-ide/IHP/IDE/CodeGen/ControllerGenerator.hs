module IHP.IDE.CodeGen.ControllerGenerator (buildPlan, buildPlan') where

import ClassyPrelude
import IHP.Prelude (textToOsPath)
import IHP.NameSupport
import IHP.HaskellSupport
import qualified Data.Text as Text
import qualified Data.Char as Char
import IHP.Postgres.Types
import IHP.IDE.CodeGen.Types
import qualified IHP.IDE.CodeGen.ViewGenerator as ViewGenerator
import Text.Countable (singularize, pluralize)

buildPlan :: Text -> Text -> Bool -> IO (Either Text [GeneratorAction])
buildPlan rawControllerName applicationName paginationEnabled = do
    schema <- loadAppSchema
    let controllerName = tableNameToControllerName rawControllerName
    let modelName = tableNameToModelName rawControllerName
    pure $ Right $ buildPlan' schema applicationName controllerName modelName paginationEnabled

buildPlan' schema applicationName controllerName modelName paginationEnabled =
    let
        config = ControllerConfig { modelName, controllerName, applicationName, paginationEnabled }
        viewPlans = generateViews schema applicationName controllerName paginationEnabled
    in
        [ CreateFile { filePath = textToOsPath (applicationName <> "/Controller/" <> controllerName <> ".hs"), fileContent = (generateController schema config) }
        , AppendToFile { filePath = textToOsPath (applicationName <> "/Routes.hs"), fileContent = "\n" <> (controllerInstance config) }
        , AppendToFile { filePath = textToOsPath (applicationName <> "/Types.hs"), fileContent = (generateControllerData config) }
        , AppendToMarker { marker = "-- Controller Imports", filePath = textToOsPath (applicationName <> "/FrontController.hs"), fileContent = ("import " <> applicationName <> ".Controller." <> controllerName) }
        , AppendToMarker { marker = "-- Generator Marker", filePath = textToOsPath (applicationName <> "/FrontController.hs"), fileContent = ("        , parseRoute @" <> controllerName <> "Controller") }
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
        name = config.controllerName
        pluralName = config.controllerName |> lcfirst |> pluralize |> ucfirst
        singularName = config.modelName |> lcfirst |> singularize |> ucfirst
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
        applicationName = config.applicationName
        name = config.controllerName
        pluralName = config.controllerName |> lcfirst |> pluralize |> ucfirst
        singularName = config.modelName |> lcfirst |> singularize |> ucfirst
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
        paginationEnabled = config.paginationEnabled

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

        modelColumns :: [Column]
        modelColumns = [ modelNameToTableName modelVariableSingular, modelVariableSingular ]
                |> mapMaybe (columnsForTable schema)
                |> headMay
                |> fromMaybe []

        modelFields :: [Text]
        modelFields = modelColumns
                |> map (.name)
                |> map columnNameToFieldName

        foreignKeys :: [(Text, Text)]
        foreignKeys = [ modelNameToTableName modelVariableSingular, modelVariableSingular ]
                |> map (foreignKeysForTable schema)
                |> concat

        extraUniqueColumns :: [Text]
        extraUniqueColumns = [ modelNameToTableName modelVariableSingular, modelVariableSingular ]
                |> map (uniqueColumnsForTable schema)
                |> concat

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
            <> validationLines

        toTypeLevelList values = "@'" <> (values |> tshow |> Text.replace "," ", ")

        validationLines :: Text
        validationLines =
            let
                fkColumnNames = map fst foreignKeys
                isTextLike = isTextLikeType . (.columnType)
                isTextLikeType PText = True
                isTextLikeType (PVaryingN _) = True
                isTextLikeType (PCharacterN _) = True
                isTextLikeType _ = False

                validationsFor col =
                    let fieldName = columnNameToFieldName col.name
                        nonEmptyLine
                            | col.notNull && isTextLike col && col.name `notElem` fkColumnNames
                            = ["    |> validateField #" <> fieldName <> " nonEmpty"]
                            | otherwise = []
                        emailLine
                            | "email" `Text.isSuffixOf` col.name && isTextLike col
                            = ["    |> validateField #" <> fieldName <> " isEmail"]
                            | otherwise = []
                        uniqueLine
                            | col.name `elem` extraUniqueColumns
                            = ["    -- TODO: |> validateIsUnique #" <> fieldName]
                            | otherwise = []
                    in nonEmptyLine <> emailLine <> uniqueLine
            in
                case concatMap validationsFor modelColumns of
                    [] -> ""
                    lines -> intercalate "\n" lines <> "\n"
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
    qualifiedModuleName config.applicationName "View" config.controllerName viewName

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
