module IHP.IDE.CodeGen.ViewGenerator (buildPlan, buildPlan', ViewConfig (..)) where

import IHP.Prelude
import qualified Data.Text as Text
import IHP.IDE.CodeGen.Types
import qualified IHP.IDE.SchemaDesigner.Parser as SchemaDesigner
import IHP.IDE.SchemaDesigner.Types
import NeatInterpolation

data ViewConfig = ViewConfig
    { controllerName :: Text
    , applicationName :: Text
    , modelName :: Text
    , viewName :: Text
    , paginationEnabled :: Bool
    } deriving (Eq, Show)

buildPlan :: Text -> Text -> Text -> IO (Either Text [GeneratorAction])
buildPlan viewName applicationName controllerName' =
    if (null viewName || null controllerName')
        then pure $ Left "Neither view name nor controller name can be empty"
        else do
            schema <- SchemaDesigner.parseSchemaSql >>= \case
                Left parserError -> pure []
                Right statements -> pure statements
            let modelName = tableNameToModelName controllerName'
            let controllerName = tableNameToControllerName controllerName'
            let paginationEnabled = False
            let viewConfig = ViewConfig { .. }
            pure $ Right $ buildPlan' schema viewConfig

-- E.g. qualifiedViewModuleName config "Edit" == "Web.View.Users.Edit"
qualifiedViewModuleName :: ViewConfig -> Text -> Text
qualifiedViewModuleName config viewName =
    get #applicationName config <> ".View." <> get #controllerName config <> "." <> viewName

buildPlan' :: [Statement] -> ViewConfig -> [GeneratorAction]
buildPlan' schema config =
        let
            controllerName = get #controllerName config
            name = get #viewName config
            singularName = config |> get #modelName |> lcfirst |> singularize |> ucfirst -- TODO: `singularize` Should Support Lower-Cased Words
            pluralName = singularName |> lcfirst |> pluralize |> ucfirst -- TODO: `pluralize` Should Support Lower-Cased Words
            singularVariableName = lcfirst singularName
            pluralVariableName = lcfirst controllerName
            nameWithSuffix = if "View" `isSuffixOf` name
                then name
                else name <> "View" --e.g. "Test" -> "TestView"
            nameWithoutSuffix = if "View" `isSuffixOf` name
                then Text.replace "View" "" name
                else name --e.g. "TestView" -> "Test"

            indexAction = pluralName <> "Action"
            specialCases = [
                  ("IndexView", indexView)
                , ("ShowView", showView)
                , ("EditView", editView)
                , ("NewView", newView)
                ]

            paginationEnabled = get #paginationEnabled config

            modelFields :: [Text]
            modelFields =  [ modelNameToTableName pluralVariableName, pluralVariableName ]
                    |> mapMaybe (fieldsForTable schema)
                    |> head
                    |> fromMaybe []

            qqClose = "|]"

            viewHeader = [text|
                module ${moduleName} where
                import ${applicationName}.View.Prelude
            |]
                where
                    moduleName = qualifiedViewModuleName config nameWithoutSuffix
                    applicationName = get #applicationName config



            genericView = [text|
                ${viewHeader}
                data ${nameWithSuffix} = {$nameWithSuffix}

                instance View ${nameWithSuffix} where
                    html ${nameWithSuffix} { .. } = [hsx|
                    <nav>
                        <ol class="breadcrumb">
                            <li class="breadcrumb-item"><a href={${indexAction}}>${pluralizedName}</a></li>
                            <li class="breadcrumb-item active">${nameWithSuffix}</li>
                        </ol>
                    </nav>
                    <h1>${nameWithSuffix}</h1>
                ${qqClose}
            |]
                where
                    pluralizedName = pluralize name


            showView =
                viewHeader
                <> "data ShowView = ShowView { " <> singularVariableName <> " :: " <> singularName <> " }\n"
                <> "\n"
                <> "instance View ShowView where\n"
                <> "    html ShowView { .. } = [hsx|\n"
                <> "        <nav>\n"
                <> "            <ol class=\"breadcrumb\">\n"
                <> "                <li class=\"breadcrumb-item\"><a href={" <> indexAction <> "}>" <> pluralName <> "</a></li>\n"
                <> "                <li class=\"breadcrumb-item active\">Show " <> singularName <> "</li>\n"
                <> "            </ol>\n"
                <> "        </nav>\n"
                <> "        <h1>Show " <> singularName <> "</h1>\n"
                <> "        <p>{" <> singularVariableName <> "}</p>\n"
                 <> "    |]\n"

            newView =
                viewHeader
                <> "data NewView = NewView { " <> singularVariableName <> " :: " <> singularName <> " }\n"
                <> "\n"
                <> "instance View NewView where\n"
                <> "    html NewView { .. } = [hsx|\n"
                <> "        <nav>\n"
                <> "            <ol class=\"breadcrumb\">\n"
                <> "                <li class=\"breadcrumb-item\"><a href={" <> indexAction <> "}>" <> pluralName <> "</a></li>\n"
                <> "                <li class=\"breadcrumb-item active\">New " <> singularName <> "</li>\n"
                <> "            </ol>\n"
                <> "        </nav>\n"
                <> "        <h1>New " <> singularName <> "</h1>\n"
                <> "        {renderForm " <> singularVariableName <> "}\n"
                <> "    |]\n"
                <> "\n"
                <> "renderForm :: " <> singularName <> " -> Html\n"
                <> "renderForm " <> singularVariableName <> " = formFor " <> singularVariableName <> " [hsx|\n"
                <> (intercalate "\n" (map (\field -> "    {(textField #" <> field <> ")}") modelFields)) <> "\n"
                <> "    {submitButton}\n"
                <> "|]\n"

            editView =
                viewHeader
                <> "data EditView = EditView { " <> singularVariableName <> " :: " <> singularName <> " }\n"
                <> "\n"
                <> "instance View EditView where\n"
                <> "    html EditView { .. } = [hsx|\n"
                <> "        <nav>\n"
                <> "            <ol class=\"breadcrumb\">\n"
                <> "                <li class=\"breadcrumb-item\"><a href={" <> indexAction <> "}>" <> pluralName <> "</a></li>\n"
                <> "                <li class=\"breadcrumb-item active\">Edit " <> singularName <> "</li>\n"
                <> "            </ol>\n"
                <> "        </nav>\n"
                <> "        <h1>Edit " <> singularName <> "</h1>\n"
                <> "        {renderForm " <> singularVariableName <> "}\n"
                <> "    |]\n"
                <> "\n"
                <> "renderForm :: " <> singularName <> " -> Html\n"
                <> "renderForm " <> singularVariableName <> " = formFor " <> singularVariableName <> " [hsx|\n"
                <> (intercalate "\n" (map (\field -> "    {(textField #" <> field <> ")}") modelFields)) <> "\n"
                <> "    {submitButton}\n"
                <> "|]\n"

            indexView =
                viewHeader
                <> "data IndexView = IndexView { " <> pluralVariableName <> " :: [" <> singularName <> "]" <> (if paginationEnabled then ", pagination :: Pagination" else "") <> " }\n"
                <> "\n"
                <> "instance View IndexView where\n"
                <> "    html IndexView { .. } = [hsx|\n"
                <> "        <nav>\n"
                <> "            <ol class=\"breadcrumb\">\n"
                <> "                <li class=\"breadcrumb-item active\"><a href={" <> indexAction <> "}>" <> pluralName <> "</a></li>\n"
                <> "            </ol>\n"
                <> "        </nav>\n"
                <> "        <h1>" <> nameWithoutSuffix <> " <a href={pathTo New" <> singularName <> "Action} class=\"btn btn-primary ml-4\">+ New</a></h1>\n"
                <> "        <div class=\"table-responsive\">\n"
                <> "            <table class=\"table\">\n"
                <> "                <thead>\n"
                <> "                    <tr>\n"
                <> "                        <th>" <> singularName <> "</th>\n"
                <> "                        <th></th>\n"
                <> "                        <th></th>\n"
                <> "                        <th></th>\n"
                <> "                    </tr>\n"
                <> "                </thead>\n"
                <> "                <tbody>{forEach " <> pluralVariableName <> " render" <> singularName <> "}</tbody>\n"
                <> "            </table>\n"
                <> (if paginationEnabled
                        then "            {renderPagination pagination}\n"
                        else "")
                <> "        </div>\n"
                <> "    |]\n"
                <> "\n\n"
                <> "render" <> singularName <> " :: " <> singularName <> " -> Html\n"
                <> "render" <> singularName <> " " <> singularVariableName <> " = [hsx|\n"
                <> "    <tr>\n"
                <> "        <td>{" <> singularVariableName <> "}</td>\n"
                <> "        <td><a href={Show" <> singularName <> "Action (get #id " <> singularVariableName <> ")}>Show</a></td>\n"
                <> "        <td><a href={Edit" <> singularName <> "Action (get #id " <> singularVariableName <> ")} class=\"text-muted\">Edit</a></td>\n"
                <> "        <td><a href={Delete" <> singularName <> "Action (get #id " <> singularVariableName <> ")} class=\"js-delete text-muted\">Delete</a></td>\n"
                <> "    </tr>\n"
                <> "|]\n"
            chosenView = fromMaybe genericView (lookup nameWithSuffix specialCases)
        in
            [ EnsureDirectory { directory = get #applicationName config <> "/View/" <> controllerName }
            , CreateFile { filePath = get #applicationName config <> "/View/" <> controllerName <> "/" <> nameWithoutSuffix <> ".hs", fileContent = chosenView }
            , AddImport { filePath = get #applicationName config <> "/Controller/" <> controllerName <> ".hs", fileContent = "import " <> qualifiedViewModuleName config nameWithoutSuffix }
            ]
