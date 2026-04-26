# Typed Router Interface

This is the user-facing API for typed GADT actions, typed request bodies,
typed forms, and typed OpenAPI docs.

For the design rationale and migration details, see
`TYPED_ROUTES_DESIGN.md`.

## Short Version

Classic IHP controllers put every constructor in one ADT:

```haskell
data ProjectsController
    = ShowProjectAction { projectId :: Id Project }
    | UpdateProjectAction { projectId :: Id Project, returnTo :: Maybe Text }
    deriving (Eq, Show, Data)
```

At the `action` method, both constructors have the same controller type. The
compiler cannot say that `ShowProjectAction` has no body while
`UpdateProjectAction` has a `ProjectInput` body.

Typed actions make the request body and response view part of the constructor
result type:

```haskell
data ProjectsAction request response where
    ShowProjectAction ::
        { projectId :: Id Project
        , includeArchived :: Maybe Bool
        } ->
        ProjectsAction 'NoBody ProjectView

    UpdateProjectAction ::
        { projectId :: Id Project
        , returnTo :: Maybe Text
        } ->
        ProjectsAction ('Body ProjectInput) ProjectView

    UploadLogoAction ::
        { projectId :: Id Project } ->
        ProjectsAction ('BodyWith UploadLogoInput '[ 'Multipart]) ProjectView
```

Path and query parameters are constructor fields. They are not the request
body. That is why the no-body constructor uses `'NoBody`, not `'NoRequest`.

Routes are declared with the normal IHP `[routes|...|]` DSL:

```haskell
$(pure [])

[routes|webRoutes
GET /projects/{projectId}?includeArchived ShowProjectAction
POST|PATCH /projects/{projectId}?returnTo UpdateProjectAction
POST /projects/{projectId}/logo           UploadLogoAction
|]

instance FrontController WebApplication where
    controllers = webRoutes
```

The route declaration is the source of truth for runtime parsing, `pathTo`,
typed form submit URLs, and OpenAPI path/query parameters.

## Body Types

`BodySpec` describes the request body:

```haskell
data BodySpec
    = NoBody
    | Body Type
    | BodyWith Type [BodyEncoding]
```

Use `'NoBody` for routes without a request body:

```haskell
ProjectsAction 'NoBody ProjectView
```

Use `'Body input` for high-level endpoints that accept both form and JSON
bodies:

```haskell
ProjectsAction ('Body ProjectInput) ProjectView
```

`'Body input` means:

```haskell
'BodyWith input '[ 'FormUrlEncoded, 'Json]
```

Use `'BodyWith input encodings` to restrict accepted encodings:

```haskell
ProjectsAction ('BodyWith ProjectInput '[ 'Json]) ProjectView
ProjectsAction ('BodyWith UploadLogoInput '[ 'Multipart]) ProjectView
```

Supported encodings are:

```haskell
'FormUrlEncoded
'Json
'Multipart
```

Runtime decoding requires matching decoder instances:

- JSON uses `FromJsonBody input`; the default instance uses `FromJSON input`
- URL-encoded forms use `FromFormBody input`; the default instance uses
  `Generic` plus `ParamReader` for record fields
- multipart uses `FromMultipartBody input`; this usually needs a manual
  instance

OpenAPI body schemas require `ToSchema input`.

## Routes

Typed GADT routes use the same quasi-quoter as classic routes. The difference
is the header:

```haskell
[routes|ProjectsController
GET /projects/{projectId} ShowProjectAction
|]
```

An uppercase header means "emit `CanRoute ProjectsController`" and is for
classic ADT controllers.

```haskell
[routes|webRoutes
GET /projects/{projectId} ShowProjectAction
|]
```

A lowercase header means "emit a top-level route list binding named
`webRoutes`". Use this form when the block contains typed GADT actions, because
the constructors have different indexed result types.

Every route line declares the HTTP method explicitly. Use `GET|POST` when a
route intentionally accepts multiple methods:

```haskell
[routes|webRoutes
GET   /projects/{projectId}/edit?returnTo  EditProjectAction
POST|PATCH /projects/{projectId}?returnTo  UpdateProjectAction
POST  /projects/{projectId}/logo           UploadLogoAction
|]
```

Path captures and query parameters are matched against constructor fields:

```haskell
data ProjectsAction request response where
    EditProjectAction ::
        { projectId :: Id Project
        , returnTo :: Maybe Text
        } ->
        ProjectsAction 'NoBody EditView
```

```haskell
[routes|webRoutes
GET /projects/{projectId}/edit?returnTo EditProjectAction
|]
```

The field type drives parsing, URL rendering, and OpenAPI schema generation.
`Maybe` query fields are optional; non-`Maybe` query fields are required.

Use explicit field bindings when the URL name differs from the record field:

```haskell
[routes|webRoutes
GET /orgs/{org}/projects/{project} ShowProjectAction { organizationId = #org, projectId = #project }
|]
```

## Writing Actions

Each concrete action index gets a `Controller` instance:

```haskell
instance Controller (ProjectsAction 'NoBody ProjectView) where
    type ControllerAction (ProjectsAction 'NoBody ProjectView) =
        ActionDef (ProjectsAction 'NoBody ProjectView) 'NoBody ProjectView

    action ShowProjectAction { projectId, includeArchived } =
        documented do
            summary "Show project"
            tags ["Projects"]
        do
            project <- fetch projectId
            pure ProjectView { projectPayload = projectPayloadFromProject project }
```

For a body action:

```haskell
instance Controller (ProjectsAction ('Body ProjectInput) ProjectView) where
    type ControllerAction (ProjectsAction ('Body ProjectInput) ProjectView) =
        ActionDef
            (ProjectsAction ('Body ProjectInput) ProjectView)
            ('Body ProjectInput)
            ProjectView

    action UpdateProjectAction { projectId, returnTo } =
        documented do
            summary "Update project"
            tags ["Projects"]
            successStatus status201
        do
            project <- fetch projectId

            project
                |> fillBody @'["title", "published"]
                |> updateRecord

            pure ProjectView
                { projectPayload =
                    ProjectPayload
                        { title = bodyParam #title
                        , published = bodyParam #published
                        }
                }
```

Use `undocumented` for handlers that should run without operation docs.

## Reading The Body

Inside a typed action, IHP decodes the body before the handler runs and exposes
it through typed helpers:

```haskell
bodyParam #title
bodyParam #published
```

These helpers are checked against the body type. If `ProjectInput` does not
have a `title` field, `bodyParam #title` does not compile.

You can also copy selected body fields into a model:

```haskell
project
    |> fillBody @'["title", "published"]
    |> updateRecord
```

Use normal constructor fields for path and query parameters:

```haskell
action ShowProjectAction { projectId, includeArchived } = do
    ...
```

## Typed JSON Responses

JSON response typing lives directly on `View`:

```haskell
data ProjectPayload = ProjectPayload
    { title :: Text
    , published :: Bool
    }
    deriving (Eq, Show, Generic)

instance ToJSON ProjectPayload
instance ToSchema ProjectPayload

data ProjectView = ProjectView
    { projectPayload :: ProjectPayload
    }

instance View ProjectView where
    html ProjectView { projectPayload } = [hsx|
        <h1>{projectPayload.title}</h1>
    |]

instance JsonView ProjectView where
    type JsonResponse ProjectView = ProjectPayload

    json ProjectView { projectPayload } = projectPayload
```

`renderHtmlOrJson` applies `toJSON` to `json view`. OpenAPI uses the same
`JsonResponse` associated type from the `JsonView` instance for the response
schema. Runtime OpenAPI validation also checks rendered JSON against the
documented typed response for documented routes.

## Forms

Typed forms use regular form field helpers against a plain input record:

```haskell
data ProjectInput = ProjectInput
    { title :: Text
    , published :: Bool
    }
    deriving (Eq, Show, Generic)
```

Define a customizable form fragment:

```haskell
projectForm :: FormMarkup ProjectInput
projectForm = [hsx|
    {(textField #title)
        { fieldLabel = "Project title"
        , placeholder = "Launch"
        , required = True
        , fieldClass = "field-title"
        , additionalAttributes = [("data-field", "title")]
        }}

    {(checkboxField #published)
        { fieldLabel = "Published?"
        }}

    {submitButton { label = "Save project" }}
|]
```

Render it against a typed action:

```haskell
formForAction
    UpdateProjectAction
        { projectId = project.id
        , returnTo = Just "/dashboard"
        }
    initialProjectInput
    projectForm
```

where `initialProjectInput` is the rendered form state:

```haskell
initialProjectInput =
    ProjectInput
        { title = project.title
        , published = project.published
        }
```

`formForAction` derives:

- submit URL from `pathTo`
- method from the route declaration
- encoding from the action body

`'Body input` uses `application/x-www-form-urlencoded` for forms.
`'BodyWith input '[ 'Multipart]` uses `multipart/form-data`. Non-GET/POST
methods submit as `POST` with IHP's existing `_method` override. JSON-only
actions do not compile with form helpers.

## OpenAPI

Typed OpenAPI docs come from the runtime contract:

- path and query parameters from `[routes|...|]`
- request body schema from `BodySpec`
- accepted request media types from `BodyEncoding`
- response schema from `JsonView.JsonResponse`
- operation metadata from the action's `documented` block

```haskell
[routes|webRoutes
PATCH /projects/{projectId}?returnTo UpdateProjectAction
|]
```

```haskell
action UpdateProjectAction { projectId, returnTo } =
    documented do
        summary "Update project"
        tags ["Projects"]
        successStatus status201
    do
        ...
```

There are no separate body, response, path-parameter, or query-parameter
declarations in the action docs. Those would duplicate the action and route
types.

Serve the generated document and Swagger UI through normal routes too:

```haskell
import IHP.OpenApiSupport
    ( OpenApiInfo (..)
    , SwaggerUiController (..)
    , SwaggerUiControllerConfig (..)
    , defaultSwaggerUiOptions
    )

[routes|openApiRoutes
GET /api-docs              SwaggerUiAction
GET /api-docs/openapi.json OpenApiJsonAction
|]
```

```haskell
instance FrontController WebApplication where
    controllers =
        webRoutes
            <> openApiRoutes
            <> [ startPage WelcomeAction ]
```

`SwaggerUiAction` renders Swagger UI. `OpenApiJsonAction` renders the generated
OpenAPI JSON. The UI links to `pathTo OpenApiJsonAction`, so changing the JSON
route in `[routes|...|]` automatically changes the URL used by Swagger UI.

Customize the generated document metadata or Swagger UI assets with a
`SwaggerUiControllerConfig` instance. The URL fields are ignored in this mode
because the route DSL owns the URLs:

```haskell
instance SwaggerUiControllerConfig WebApplication where
    swaggerUiControllerOptions =
        (defaultSwaggerUiOptions @WebApplication)
            { swaggerUiTitle = Just "My API"
            , swaggerUiInfo =
                OpenApiInfo
                    { openApiTitle = "My API"
                    , openApiVersion = "1.0.0"
                    , openApiDescription = Just "Public API"
                    }
            }
```

## Migration

Use typed routes where the extra type information pays for itself:

- JSON/form APIs
- generated OpenAPI
- generated forms
- routes with multiple body encodings
- endpoints where body and response drift has caused bugs

Classic controllers can still serve ordinary pages, but the typed request body,
typed form, and OpenAPI contract is GADT-only. There is no `ActionDefinition`,
`endpoint`, or `documentRoute` migration path.

Migration steps:

1. Split the classic controller ADT into a GADT with `request` and `response`
   indices.
2. Create input record types for body-bearing routes.
3. Move body reads from `param "field"` to `bodyParam #field` or `fillBody`.
4. Declare routes in a lowercase `[routes|webRoutes ...|]` block.
5. Mount with `controllers = webRoutes`.
6. Move OpenAPI metadata into `documented`, leaving path/query/body/response
   schemas to the typed route and action types.

## Limitations

The typed routes DSL currently supports static path segments, typed path
captures, splats, and typed query parameters. A GADT route block must use a
lowercase binding header because it emits a heterogeneous route list instead of
a `CanRoute` instance for one concrete controller type.

Some custom route parameter types may need `UrlCapture`, `ToSchema`, and
`DummyRouteValue` instances.
