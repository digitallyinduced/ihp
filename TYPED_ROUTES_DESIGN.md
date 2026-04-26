# Typed Routes Design

This document explains why typed GADT actions now use IHP's `[routes|...|]`
DSL, how that DSL was changed, and how this differs from `AutoRoute`, custom
parser routes, and Servant.

For the user-facing API, see `TYPED_ROUTER_INTERFACE.md`.

## What Changed

Typed GADT actions are declared with the normal IHP route DSL:

```haskell
$(pure [])

[routes|webRoutes
GET /projects/{projectId}?includeArchived ShowProjectAction
PATCH /projects/{projectId}?returnTo   UpdateProjectAction
POST /projects/{projectId}/logo        UploadLogoAction
|]

instance FrontController WebApplication where
    controllers = webRoutes
```

The route block now supports GADT action constructors whose result type is
indexed by request body and response view:

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
```

The route declaration is now the single source of truth for:

- runtime route matching
- path and query parameter parsing
- `pathTo`
- typed form submit URL and method selection
- OpenAPI path template
- OpenAPI path/query parameter docs

## Why The Existing DSL Needed Changes

Upstream IHP already had two route declaration mechanisms for ordinary
controllers:

- `AutoRoute`, which derives conventional routes from a classic ADT
- `[routes|...|]`, which declares explicit routes and emits route tries

Both were built around one concrete controller type:

```haskell
data ProjectsController
    = ShowProjectAction { projectId :: Id Project }
    | UpdateProjectAction { projectId :: Id Project }
```

For that shape, the DSL can emit:

```haskell
instance HasPath ProjectsController
instance CanRoute ProjectsController
```

Typed actions have a different shape. The constructor result type contains
the request body and response view:

```haskell
ProjectsAction 'NoBody ProjectView
ProjectsAction ('Body ProjectInput) ProjectView
ProjectsAction ('BodyWith UploadLogoInput '[ 'Multipart]) ProjectView
```

There is no single concrete `ProjectsAction` value type to put behind
`CanRoute`. A route list can contain constructors with different indices, and
dispatch needs to keep those indices until the handler runs so request decoding,
response rendering, generated forms, and OpenAPI all see the same type
evidence.

The solution is to keep the route syntax and router trie, but change the TH
emitter:

- ordinary ADT route blocks still emit `CanRoute`
- GADT route blocks emit per-constructor `HasPath` instances
- lowercase route blocks emit a ready-to-mount `webRoutes` binding
- each GADT route entry stores a route-specific runner with the constructor's
  body and response types

## Lowercase Bindings

Classic controller block:

```haskell
[routes|ProjectsController
GET /projects/{projectId} ShowProjectAction
|]
```

This emits `CanRoute ProjectsController`, so it can be mounted with:

```haskell
controllers = [parseRoute @ProjectsController]
```

Typed GADT block:

```haskell
[routes|webRoutes
GET /projects/{projectId} ShowProjectAction
PATCH /projects/{projectId} UpdateProjectAction
|]
```

This emits a top-level route list:

```haskell
webRoutes :: ... => [ControllerRoute app]
```

and is mounted with:

```haskell
controllers = webRoutes
```

The lowercase binding form is required for GADT action families because a
single `parseRoute @ProjectsAction` cannot name all constructor indices.

## Methods

Every route line declares its HTTP method explicitly:

```haskell
[routes|webRoutes
GET   /projects/{projectId}/edit EditProjectAction
POST|PATCH /projects/{projectId} UpdateProjectAction
|]
```

Use `GET|POST` when one route intentionally accepts multiple methods. `GET`
routes are also registered for `HEAD`, matching existing IHP behavior.

## Path And Query Parameters

Path captures and query params are declared in the route:

```haskell
[routes|webRoutes
GET /projects/{projectId}/edit?returnTo EditProjectAction
|]
```

and must correspond to fields on the constructor:

```haskell
EditProjectAction ::
    { projectId :: Id Project
    , returnTo :: Maybe Text
    } ->
    ProjectsAction 'NoBody EditView
```

The field type drives `UrlCapture` parsing/rendering and OpenAPI schemas.
`Maybe` query fields are optional; non-`Maybe` query fields are required.

If URL names and field names differ, use explicit bindings:

```haskell
[routes|webRoutes
GET /orgs/{org}/projects/{project} ShowProjectAction { organizationId = #org, projectId = #project }
|]
```

## OpenAPI Coupling

OpenAPI route metadata is generated from the same values used at runtime:

- path template from the `[routes|...|]` path
- path/query parameter schemas from constructor field types
- request body schema from the GADT body index
- request media types from the body encoding index
- response schema from `JsonView.JsonResponse`
- summary/tags/status/descriptions from the action's `documented` block

That means the action docs no longer contain separate body, response, path
parameter, or query parameter declarations.

Those helpers would duplicate type information that the route/action contract
already knows.

## Why Not AutoRoute

`AutoRoute` derives routes from constructor names and `Data`. It works well for
classic ADTs, but it does not preserve per-constructor body and response
indices. Once a GADT constructor is erased into a generic route parser, the
framework no longer has the type evidence needed to decode the right body or
document the right response.

Typed actions need a route entry that carries the concrete constructor result
type all the way into dispatch.

## Why Not Custom Parser Routes

Custom parser routes solve path parsing only:

```haskell
instance CanRoute ProjectsController where
    parseRoute' = ...
```

They do not give `pathTo`, OpenAPI path templates, query parameter docs, typed
form methods, or request/response type evidence. You can still use custom
parser routes for classic controllers, but they are not the no-drift path for
typed actions.

## Difference With Servant

Servant puts the API at the type level:

```haskell
type API =
    "projects" :> Capture "projectId" (Id Project) :> ReqBody '[JSON] ProjectInput :> Post '[JSON] ProjectPayload
```

Handlers are then checked against that API type.

IHP keeps the existing MVC shape:

- routes stay in `[routes|...|]`
- actions stay as controller constructors
- request body and response view live on the GADT constructor result type
- views still render HTML and JSON through `View`

So this is not a Servant-style API-first framework. It is IHP's controller
model with stronger per-action type evidence and generated OpenAPI/forms tied
to the same runtime route declaration.

## Route Declaration Shape

```haskell
[routes|webRoutes
GET /projects/{projectId}?includeArchived ShowProjectAction
|]

instance FrontController WebApplication where
    controllers = webRoutes
```

The route block owns route shape, query parameters, and methods. The
GADT action definitions and `Controller` instances own request/response typing
and handler behavior.

## Current Limitations

Typed GADT route blocks must use a lowercase binding header. Uppercase headers
still mean "emit `CanRoute` for this one concrete controller type", which does
not fit indexed constructors.

The DSL supports static segments, captures, splats, and query parameters. Very
unusual routing behavior may still require classic custom parser routes, but
that path does not provide typed action OpenAPI/form coupling.

Custom parameter types need the same instances used by the route generator:

- `UrlCapture` for parsing and rendering
- `ToSchema` for OpenAPI
- `DummyRouteValue` when a dummy action value is needed for documentation
