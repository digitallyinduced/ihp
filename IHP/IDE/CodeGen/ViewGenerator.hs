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

            -- when using the trimming quasiquoter we can't have another |] closure, like for the one we use with hsx.
            qqClose = "|]"

            viewHeader = [trimming|
                module ${moduleName} where
                import ${applicationName}.View.Prelude
            |]
                where
                    moduleName = qualifiedViewModuleName config nameWithoutSuffix
                    applicationName = get #applicationName config



            genericView = [trimming|
                ${viewHeader}
                data ${nameWithSuffix} = {$nameWithSuffix}

                instance View ${nameWithSuffix} where
                    html ${nameWithSuffix} { .. } = [hsx|
                        {breadcrumbs}
                        <h1>${nameWithSuffix}</h1>
                        ${qqClose}
                            where
                                breadcrumbs = renderBreadcrumbs
                                                [ BreadcrumbsItem { label = "${pluralizedName}", url = Just $ pathTo ${indexAction}, isActive = False }
                                                , BreadcrumbsItem { label = "${nameWithSuffix}", url = Nothing , isActive = True}
                                                ]
            |]
                where
                    pluralizedName = pluralize name


            showView = [trimming|
                ${viewHeader}

                data ShowView = ShowView { ${singularVariableName} :: ${singularName} }

                instance View ShowView where
                    html ShowView { .. } = [hsx|
                        {breadcrumbs}
                        <h1>Show ${singularName}</h1>
                        <p>{${singularVariableName}}</p>

                    ${qqClose}
                        where
                            breadcrumbs = renderBreadcrumbs
                                            [ BreadcrumbsItem { label = "${pluralName}", url = Just $ pathTo ${indexAction}, isActive = False }
                                            , BreadcrumbsItem { label = [hsx|Show ${singularName}${qqClose}, url = Nothing , isActive = True}
                                            ]
            |]

            -- The form that will appear in New and Edit pages.
            renderForm = [trimming|
                renderForm :: ${singularName} -> Html
                renderForm ${singularVariableName} = formFor ${singularVariableName} [hsx|
                    ${formFields}
                    {submitButton}

                ${qqClose}
            |]
                where
                    formFields =
                        intercalate "\n" (map (\field -> "    {(textField #" <> field <> ")}") modelFields)


            newView = [trimming|
                ${viewHeader}

                data NewView = NewView { ${singularVariableName} :: ${singularName} }

                instance View NewView where
                    html NewView { .. } = [hsx|
                        {breadcrumbs}
                        <h1>New ${singularName}</h1>
                        {renderForm ${singularVariableName}}
                    ${qqClose}
                        where
                            breadcrumbs = renderBreadcrumbs
                                [ BreadcrumbsItem { label = "${pluralName}", url = Just $ pathTo ${indexAction}, isActive = False }
                                , BreadcrumbsItem { label = [hsx|New ${singularName}${qqClose}, url = Nothing , isActive = True}
                                ]

                ${renderForm}
            |]

            editView = [trimming|
                ${viewHeader}

                data EditView = EditView { ${singularVariableName} :: ${singularName} }

                instance View EditView where
                    html EditView { .. } = [hsx|
                        {breadcrumbs}
                        <h1>Edit ${singularName}</h1>
                        {renderForm ${singularVariableName}}
                    ${qqClose}
                        where
                            breadcrumbs = renderBreadcrumbs
                                [ BreadcrumbsItem { label = "${pluralName}", url = Just $ pathTo ${indexAction}, isActive = False }
                                , BreadcrumbsItem { label = [hsx|Edit ${singularName}${qqClose}, url = Nothing , isActive = True}
                                ]

                ${renderForm}
            |]

            indexView = [trimming|
                ${viewHeader}

                data IndexView = IndexView { ${pluralVariableName} :: [ ${singularName} ] ${importPagination} }

                instance View IndexView where
                    html IndexView { .. } = [hsx|
                        <h1>${nameWithoutSuffix}<a href={pathTo New${singularName}Action} class="btn btn-primary ml-4">+ New</a></h1>
                        <div class="table-responsive">
                            <table class="table">
                                <thead>
                                    <tr>
                                        <th>${singularName}</th>
                                        <th></th>
                                        <th></th>
                                        <th></th>
                                    </tr>
                                </thead>
                                <tbody>{forEach ${pluralVariableName} render${singularName}}</tbody>
                            </table>
                            ${renderPagination}
                        </div>
                    ${qqClose}
                        where
                            breadcrumbs = renderBreadcrumbs
                                [ BreadcrumbsItem { label = "${pluralName}", url =  Just $ pathTo ${indexAction} , isActive = True}
                                ]

                    render${singularName} :: ${singularName} -> Html
                    render${singularName} ${singularVariableName} = [hsx|
                        <tr>
                            <td>{${singularVariableName}}</td>
                            <td><a href={Show${singularName}Action (get #id ${singularVariableName})}>Show</a></td>
                            <td><a href={Edit${singularName}Action (get #id ${singularVariableName})} class="text-muted">Edit</a></td>
                            <td><a href={Delete${singularName}Action (get #id ${singularVariableName})} class="js-delete text-muted">Delete</a></td>
                        </tr>
                    ${qqClose}
            |]
                where
                    importPagination = if paginationEnabled then ", pagination :: Pagination" else ""
                    renderPagination = if paginationEnabled then "{renderPagination pagination}" else ""



            chosenView = fromMaybe genericView (lookup nameWithSuffix specialCases)
        in
            [ EnsureDirectory { directory = get #applicationName config <> "/View/" <> controllerName }
            , CreateFile { filePath = get #applicationName config <> "/View/" <> controllerName <> "/" <> nameWithoutSuffix <> ".hs", fileContent = chosenView }
            , AddImport { filePath = get #applicationName config <> "/Controller/" <> controllerName <> ".hs", fileContent = "import " <> qualifiedViewModuleName config nameWithoutSuffix }
            ]
