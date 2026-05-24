module IHP.IDE.CodeGen.ControllerGenerator (buildPlan, buildPlan', ControllerConfig(..), defaultControllerConfig) where

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

buildPlan :: Text -> ControllerConfig -> IO (Either Text [GeneratorAction])
buildPlan rawControllerName config = do
    schema <- loadAppSchema
    let controllerName = tableNameToControllerName rawControllerName
    let modelName = tableNameToModelName rawControllerName
    pure $ Right $ buildPlan' schema config { controllerName, modelName }

buildPlan' :: [Statement] -> ControllerConfig -> [GeneratorAction]
buildPlan' schema config =
    let
        viewPlans = generateViews schema config
        singularName = config.modelName |> lcfirst |> singularize |> ucfirst
        modelVariableSingular = lcfirst singularName
        -- Whether the model's table exists in the schema decides whether
        -- Show/Edit/Update/Delete action constructors carry an id field.
        -- When False, those actions are emitted as nullary constructors,
        -- so the routes block must NOT declare an ?idField query param
        -- (the TH splice would fail the coverage check).
        tableFound = [ modelNameToTableName modelVariableSingular, modelVariableSingular ]
                |> mapMaybe (columnsForTable schema)
                |> headMay
                |> isJust
    in
        [ CreateFile { filePath = textToOsPath (config.applicationName <> "/Controller/" <> config.controllerName <> ".hs"), fileContent = (generateController schema config) }
        , AppendToFile { filePath = textToOsPath (config.applicationName <> "/Routes.hs"), fileContent = "\n" <> controllerInstance config tableFound }
        , AppendToFile { filePath = textToOsPath (config.applicationName <> "/Types.hs"), fileContent = (generateControllerData schema config) }
        , AppendToMarker { marker = "-- Controller Imports", filePath = textToOsPath (config.applicationName <> "/FrontController.hs"), fileContent = ("import " <> config.applicationName <> ".Controller." <> config.controllerName) }
        , AppendToMarker { marker = "-- Generator Marker", filePath = textToOsPath (config.applicationName <> "/FrontController.hs"), fileContent = ("        , parseRoute @" <> config.controllerName <> "Controller") }
        ]
        <> viewPlans

data ControllerConfig = ControllerConfig
    { controllerName :: Text
    , applicationName :: Text
    , modelName :: Text
    , paginationEnabled :: Bool
    , indexActionEnabled :: Bool
    , newActionEnabled :: Bool
    , showActionEnabled :: Bool
    , createActionEnabled :: Bool
    , editActionEnabled :: Bool
    , updateActionEnabled :: Bool
    , deleteActionEnabled :: Bool
    } deriving (Eq, Show)

defaultControllerConfig :: ControllerConfig
defaultControllerConfig = ControllerConfig
    { controllerName = ""
    , applicationName = "Web"
    , modelName = ""
    , paginationEnabled = False
    , indexActionEnabled = True
    , newActionEnabled = True
    , showActionEnabled = True
    , createActionEnabled = True
    , editActionEnabled = True
    , updateActionEnabled = True
    , deleteActionEnabled = True
    }

-- | Emit the per-controller @[routes|…|]@ block appended to
-- @<Application>/Routes.hs@ by the controller generator.
--
-- The URL shapes mirror AutoRoute's defaults so freshly-generated apps
-- keep the /Posts, /NewPost, /ShowPost?postId=<uuid> style that existing
-- docs, tutorials, and deep links depend on. HTTP methods follow
-- AutoRoute's name-prefix heuristic (GET for index/new/show/edit, POST
-- for create/update, DELETE for delete).
--
-- Only the actions enabled in the 'ControllerConfig' are emitted — the
-- scaffolder lets the user switch any subset off.
--
-- When @tableFound@ is 'False', the matching model table isn't in the
-- schema, so 'generateControllerData' emits nullary Show/Edit/Update/
-- Delete constructors (no @{idField :: Id …}@). In that case the routes
-- block must NOT carry @?idField@ either, or the TH splice's coverage
-- check would fail ("query parameter has no matching field").
controllerInstance :: ControllerConfig -> Bool -> Text
controllerInstance config@ControllerConfig { controllerName, modelName } tableFound =
    let singularName = modelName |> lcfirst |> singularize |> ucfirst
        pluralName   = controllerName |> lcfirst |> pluralize |> ucfirst
        idFieldName  = lcfirst singularName <> "Id"

        -- Append ?idField only if the schema gave the action an id field.
        withId path
            | tableFound = path <> "?" <> idFieldName
            | otherwise  = path

        line enabled method path actionName
            | enabled   = Just (method <> " " <> path <> " " <> actionName)
            | otherwise = Nothing

        lines = catMaybes
            [ line config.indexActionEnabled "GET"    ("/" <> pluralName)                          (pluralName <> "Action")
            , line config.newActionEnabled   "GET"    ("/New" <> singularName)                     ("New" <> singularName <> "Action")
            , line config.createActionEnabled "POST"  ("/Create" <> singularName)                  ("Create" <> singularName <> "Action")
            , line config.showActionEnabled  "GET"    (withId ("/Show" <> singularName))           ("Show" <> singularName <> "Action")
            , line config.editActionEnabled  "GET"    (withId ("/Edit" <> singularName))           ("Edit" <> singularName <> "Action")
            , line config.updateActionEnabled "POST"  (withId ("/Update" <> singularName))         ("Update" <> singularName <> "Action")
            , line config.deleteActionEnabled "DELETE" (withId ("/Delete" <> singularName))        ("Delete" <> singularName <> "Action")
            ]
    in
        "[routes|" <> controllerName <> "Controller\n"
        <> Text.intercalate "\n" lines <> "\n"
        <> "|]\n\n"

data HaskellModule = HaskellModule { moduleName :: Text, body :: Text }

generateControllerData :: [Statement] -> ControllerConfig -> Text
generateControllerData schema config =
    let
        name = config.controllerName
        pluralName = config.controllerName |> lcfirst |> pluralize |> ucfirst
        singularName = config.modelName |> lcfirst |> singularize |> ucfirst
        modelVariableSingular = lcfirst singularName
        idFieldName = lcfirst singularName <> "Id"
        idType = "Id " <> singularName
        tableFound = [ modelNameToTableName modelVariableSingular, modelVariableSingular ]
                |> mapMaybe (columnsForTable schema)
                |> headMay
                |> isJust
        idRecord suffix = if tableFound
            then suffix <> " { " <> idFieldName <> " :: !(" <> idType <> ") }"
            else suffix
        constructors = catMaybes
            [ if config.indexActionEnabled then Just (pluralName <> "Action") else Nothing
            , if config.newActionEnabled then Just ("New" <> singularName <> "Action") else Nothing
            , if config.showActionEnabled then Just (idRecord $ "Show" <> singularName <> "Action") else Nothing
            , if config.createActionEnabled then Just ("Create" <> singularName <> "Action") else Nothing
            , if config.editActionEnabled then Just (idRecord $ "Edit" <> singularName <> "Action") else Nothing
            , if config.updateActionEnabled then Just (idRecord $ "Update" <> singularName <> "Action") else Nothing
            , if config.deleteActionEnabled then Just (idRecord $ "Delete" <> singularName <> "Action") else Nothing
            ]
        formattedConstructors = case constructors of
            (first:rest) ->
                "    = " <> first <> "\n"
                <> concatMap (\c -> "    | " <> c <> "\n") rest
                <> "    deriving (Eq, Show, Data)\n"
            [] -> "    deriving (Eq, Show, Data)\n"
    in
        "\n"
        <> "data " <> name <> "Controller\n"
        <> formattedConstructors

generateController :: [Statement] -> ControllerConfig -> Text
generateController schema config =
    let
        applicationName = config.applicationName
        name = config.controllerName
        pluralName = config.controllerName |> lcfirst |> pluralize |> ucfirst
        singularName = config.modelName |> lcfirst |> singularize |> ucfirst
        moduleName =  applicationName <> ".Controller." <> name
        controllerName = name <> "Controller"

        -- Create/Update reference NewView/EditView on validation errors
        needsNewView = config.newActionEnabled || config.createActionEnabled
        needsEditView = config.editActionEnabled || config.updateActionEnabled
        needsIndexView = config.indexActionEnabled
        needsShowView = config.showActionEnabled

        importStatements = catMaybes
            [ Just ("import " <> applicationName <> ".Controller.Prelude")
            , if needsIndexView then Just ("import " <> qualifiedViewModuleName config "Index") else Nothing
            , if needsNewView then Just ("import " <> qualifiedViewModuleName config "New") else Nothing
            , if needsEditView then Just ("import " <> qualifiedViewModuleName config "Edit") else Nothing
            , if needsShowView then Just ("import " <> qualifiedViewModuleName config "Show") else Nothing
            ]

        modelVariablePlural = lcfirst name
        modelVariableSingular = lcfirst singularName
        idFieldName = lcfirst singularName <> "Id"
        model = ucfirst singularName
        paginationEnabled = config.paginationEnabled

        actionBodyConfig = ActionBodyConfig { singularName, modelVariableSingular, idFieldName, model, indexAction = pluralName <> "Action", tableFound }

        indexAction =
            ""
            <> "    action " <> pluralName <> "Action = do\n"
            <> (if paginationEnabled
                then   "        (" <> modelVariablePlural <> "Q, pagination) <- query @" <> model <> " |> paginate\n"
                    <> "        " <> modelVariablePlural <> " <- " <> modelVariablePlural <> "Q |> fetch\n"
                else "        " <> modelVariablePlural <> " <- query @" <> model <> " |> fetch\n"
            )
            <> "        render IndexView { .. }\n"

        newAction = generateNewActionBody actionBodyConfig
        showAction = generateShowActionBody actionBodyConfig
        editAction = generateEditActionBody actionBodyConfig

        tableFound :: Bool
        tableFound = [ modelNameToTableName modelVariableSingular, modelVariableSingular ]
                |> mapMaybe (columnsForTable schema)
                |> headMay
                |> isJust

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

        updateAction = generateUpdateActionBody actionBodyConfig
        createAction = generateCreateActionBody actionBodyConfig
        deleteAction = generateDeleteActionBody actionBodyConfig

        needsBuildModel = config.createActionEnabled || config.updateActionEnabled

        fromParams =
            if needsBuildModel
            then ""
                <> "build" <> singularName <> " " <> modelVariableSingular <> " = " <> modelVariableSingular <> "\n"
                <> "    |> fill " <> toTypeLevelList modelFields <> "\n"
                <> validationLines
            else ""

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

        actionBodies = catMaybes
            [ if config.indexActionEnabled then Just indexAction else Nothing
            , if config.newActionEnabled then Just newAction else Nothing
            , if config.showActionEnabled then Just showAction else Nothing
            , if config.editActionEnabled then Just editAction else Nothing
            , if config.updateActionEnabled then Just updateAction else Nothing
            , if config.createActionEnabled then Just createAction else Nothing
            , if config.deleteActionEnabled then Just deleteAction else Nothing
            ]
    in
        ""
        <> "module " <> moduleName <> " where" <> "\n"
        <> "\n"
        <> intercalate "\n" importStatements
        <> "\n\n"
        <> "instance Controller " <> controllerName <> " where\n"
        <> intercalate "\n" actionBodies
        <> "\n"
        <> fromParams

-- E.g. qualifiedViewModuleName config "Edit" == "Web.View.Users.Edit"
qualifiedViewModuleName :: ControllerConfig -> Text -> Text
qualifiedViewModuleName config viewName =
    qualifiedModuleName config.applicationName "View" config.controllerName viewName

pathToModuleName :: Text -> Text
pathToModuleName moduleName = Text.replace "." "/" moduleName

generateViews :: [Statement] -> ControllerConfig -> [GeneratorAction]
generateViews schema config =
    if null config.controllerName
        then []
        else do
            let needsIndexView = config.indexActionEnabled
            let needsNewView = config.newActionEnabled || config.createActionEnabled
            let needsShowView = config.showActionEnabled
            let needsEditView = config.editActionEnabled || config.updateActionEnabled
            concat $ catMaybes
                [ if needsIndexView then Just (ViewGenerator.buildPlan' schema (viewConfig "IndexView")) else Nothing
                , if needsNewView then Just (ViewGenerator.buildPlan' schema (viewConfig "NewView")) else Nothing
                , if needsShowView then Just (ViewGenerator.buildPlan' schema (viewConfig "ShowView")) else Nothing
                , if needsEditView then Just (ViewGenerator.buildPlan' schema (viewConfig "EditView")) else Nothing
                ]
    where
        viewConfig viewName =
            let modelName = config.modelName
                controllerName = config.controllerName
                applicationName = config.applicationName
                paginationEnabled = config.paginationEnabled
            in ViewGenerator.ViewConfig { .. }


isAlphaOnly :: Text -> Bool
isAlphaOnly text = Text.all (\c -> Char.isAlpha c || c == '_') text
