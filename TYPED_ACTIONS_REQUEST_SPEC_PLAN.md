# Typed Actions and Unified Request Specs Plan

## Goal

Introduce an opt-in typed action architecture where one contract drives
routing, URL generation, request decoding, form rendering, handler types, and
OpenAPI documentation.

## Current Direction

Typed GADT actions use IHP's normal `[routes|...|]` DSL.

The route DSL is responsible for:

- runtime route matching
- path/query parameter parsing
- `pathTo`
- typed form submit URL and method lookup
- OpenAPI path templates
- OpenAPI path/query parameter docs

The GADT action result type is responsible for:

- request body type
- accepted body encodings
- response view type
- typed request decoding before the handler runs
- OpenAPI request/response schemas

The action's `documented` block is responsible only for operation metadata:

- summary
- description
- tags
- operation id
- success status
- response description

It should not repeat body, response, path, or query schemas.

## User-Facing Shape

```haskell
data ProjectsAction request response where
    EditProjectAction ::
        { projectId :: Id Project
        , returnTo :: Maybe Text
        , tab :: ProjectSettingsTab
        } ->
        ProjectsAction 'NoBody EditView

    UpdateProjectAction ::
        { projectId :: Id Project
        , returnTo :: Maybe Text
        , tab :: ProjectSettingsTab
        } ->
        ProjectsAction ('Body ProjectSettingsInput) EditView

    UploadProjectLogoAction ::
        { projectId :: Id Project } ->
        ProjectsAction ('BodyWith LogoUploadInput '[ 'Multipart]) EditView
```

```haskell
$(pure [])

[routes|webRoutes
GET /projects/{projectId}/edit?returnTo&tab EditProjectAction
POST|PATCH /projects/{projectId}?returnTo&tab UpdateProjectAction
POST /projects/{projectId}/logo             UploadProjectLogoAction
|]

instance FrontController WebApplication where
    controllers = webRoutes
```

Each route line declares its HTTP method explicitly. Use `GET|POST` when a
route intentionally accepts multiple methods.

## Body Model

Do not expose separate public concepts named `JsonBody` and `FormBody`.
Expose a high-level body spec:

```haskell
data BodyEncoding
    = Json
    | FormUrlEncoded
    | Multipart

data BodySpec
    = NoBody
    | Body Type
    | BodyWith Type [BodyEncoding]

type DefaultBodyEncodings = '[ 'FormUrlEncoded, 'Json ]
```

`'Body input` means `input` can be decoded from both
`application/x-www-form-urlencoded` and `application/json`.

Use `'BodyWith input encodings` only when the accepted encodings are narrower
or special:

```haskell
CreateProjectApiAction ::
    ProjectsAction ('BodyWith CreateProjectRequest '[ 'Json]) AckView

UploadProjectLogoAction ::
    { projectId :: Id Project } ->
    ProjectsAction ('BodyWith LogoUploadInput '[ 'Multipart]) EditView
```

## Handler Model

Handlers keep IHP's familiar action style. The body is decoded by dispatch and
read through typed helpers:

```haskell
action UpdateProjectAction { projectId, returnTo, tab } =
    documented do
        summary "Update project"
        tags ["Projects"]
        successStatus status201
    do
        project <- fetch projectId

        project
            |> fillBody @'["name", "enabled"]
            |> updateRecord

        pure EditView
            { project
            , tab
            , returnTo
            }
```

`bodyParam #name` and `fillBody @'["name"]` are checked against the GADT body
type. Path/query params remain constructor fields.

## Forms

Typed forms render against an action value and an initial input record:

```haskell
formForAction
    UpdateProjectAction
        { projectId = project.id
        , returnTo = Just "/dashboard"
        , tab = GeneralTab
        }
    initialProjectSettingsInput
    projectSettingsForm
```

The input record is only the rendered form state used to fill values,
checkboxes, and selected options. The submitted body still comes from the
browser form fields and is decoded into the GADT body type.

The helper derives:

- `action` URL from `pathTo`
- form method from the route declaration
- `_method` override for PATCH/PUT/DELETE
- `enctype` from the body encodings

JSON-only actions do not compile with form helpers.

## OpenAPI

OpenAPI should be generated from the runtime contract:

- `[routes|...|]` gives path templates and path/query parameter schemas
- `BodySpec` gives request body schema and media types
- `JsonView.JsonResponse` gives response schema
- `documented` gives operation metadata

This avoids drift because there is no separate action-level request-body,
response-view, path-parameter, or query-parameter declaration.

## Compatibility

Classic IHP controllers remain supported:

- `AutoRoute`
- uppercase `[routes|ControllerName ...|]`
- custom parser routes
- classic `formFor`
- classic `param`/`fill`

Typed GADT actions use lowercase `[routes|webRoutes ...|]` blocks because the
generated value is a heterogeneous route list, not a `CanRoute` instance for
one concrete controller type.

## Implementation Work

Done in this branch:

- `BodySpec`, `BodyEncoding`, default JSON + form decoding, explicit
  `BodyWith` encodings
- typed `ActionDef` with `documented`/`undocumented`
- typed body helpers `bodyParam` and `fillBody`
- typed `JsonView.json` response schemas via `JsonView.JsonResponse`
- runtime OpenAPI render validation for documented typed routes
- typed action forms via `formForAction`/`formForActionWithOptions` using the
  regular form field helpers
- multipart typed forms and OpenAPI media types
- GADT constructor support in `ihp-router` route reification
- lowercase `[routes|webRoutes ...|]` bindings for typed GADT route lists
- route-derived path/query OpenAPI docs
- typed route dispatch through the existing trie runtime

Still planned:

- validation/error rendering hooks for typed form inputs
- higher-level multipart upload helpers on top of the low-level typed file
  field
- typed CRUD generator output
