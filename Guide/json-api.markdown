# JSON API

```toc

```

## Introduction

IHP can serve JSON responses alongside regular HTML views. This is useful when you need to build API endpoints for mobile apps, single-page applications (SPAs), or third-party integrations.

This guide shows you how to return JSON from controllers, read JSON request bodies, build a full REST API, configure routing and CORS, return proper error responses, and handle authentication for API endpoints.

## Returning JSON Responses

The simplest way to return JSON from a controller action is `renderJson`. It takes any value that has a `ToJSON` instance and sends it as an `application/json` response:

```haskell
module Web.Controller.Posts where

import Web.Controller.Prelude

instance Controller PostsController where
    action PostsAction = do
        renderJson (object ["status" .= ("ok" :: Text), "message" .= ("Hello from IHP!" :: Text)])
```

The `Data.Aeson` module (which provides `object`, `.=`, `ToJSON`, `FromJSON`, etc.) is already re-exported by `Web.Controller.Prelude`, so you do not need any extra imports for basic JSON work.

You can return any value that implements `ToJSON` -- strings, numbers, booleans, lists, maps, or your own types:

```haskell
action PostsAction = do
    renderJson ("Hello" :: Text)
    -- Response: "Hello"

action PostsAction = do
    renderJson (42 :: Int)
    -- Response: 42

action PostsAction = do
    renderJson (object ["users" .= (["alice", "bob"] :: [Text])])
    -- Response: {"users":["alice","bob"]}
```

## Adding ToJSON Instances

IHP database records do not have `ToJSON` instances by default. You need to define them yourself, which gives you full control over exactly what fields appear in your JSON output.

### Manual ToJSON Instance

Suppose you have a `posts` table with columns `id`, `title`, `body`, and `created_at`. You can write a `ToJSON` instance like this:

```haskell
instance ToJSON Post where
    toJSON post = object
        [ "id" .= post.id
        , "title" .= post.title
        , "body" .= post.body
        , "createdAt" .= post.createdAt
        ]
```

Place this instance in the controller file or view file where you need it. If multiple controllers need the same instance, you can put it in a shared module (e.g. `Web/Controller/Prelude.hs` or a dedicated `Web/JsonInstances.hs`).

Now you can return posts as JSON:

```haskell
action PostsAction = do
    posts <- query @Post |> fetch
    renderJson posts
```

This will produce a response like:

```json
[
  {
    "id": "5a8a4c5e-1b5e-4b2a-9c0a-3e5e4b8e1b5e",
    "title": "My First Post",
    "body": "Hello, world!",
    "createdAt": "2025-01-15T10:30:00Z"
  }
]
```

### Controlling JSON Field Names

You have full control over the JSON keys. For example, if your API consumers expect `snake_case` keys:

```haskell
instance ToJSON Post where
    toJSON post = object
        [ "id" .= post.id
        , "title" .= post.title
        , "body" .= post.body
        , "created_at" .= post.createdAt
        ]
```

### Nested Objects

You can nest objects and include related data:

```haskell
instance ToJSON Post where
    toJSON post = object
        [ "id" .= post.id
        , "title" .= post.title
        , "body" .= post.body
        , "author" .= object
            [ "id" .= post.authorId
            ]
        ]
```

### FromJSON Instances

If you need to parse incoming JSON into a custom type (not an IHP record), define a `FromJSON` instance:

```haskell
data CreatePostPayload = CreatePostPayload
    { title :: Text
    , body :: Text
    }

instance FromJSON CreatePostPayload where
    parseJSON = withObject "CreatePostPayload" \o -> do
        title <- o .: "title"
        body <- o .: "body"
        pure CreatePostPayload { title, body }
```

## Reading JSON Request Bodies

### Using param with JSON Bodies

When a client sends a request with `Content-Type: application/json`, IHP automatically parses the JSON body. The `param` function works with both form-encoded and JSON request bodies. For a JSON body like:

```json
{
  "title": "My Post",
  "body": "Hello, world!"
}
```

You can read individual fields the same way you would with form parameters:

```haskell
action CreatePostAction = do
    let title = param @Text "title"
    let body = param @Text "body"
    ...
```

This means `fill` also works with JSON request bodies:

```haskell
action CreatePostAction = do
    let post = newRecord @Post
    post
        |> fill @'["title", "body"]
        |> ifValid \case
            Left post -> renderJson (object ["error" .= ("Validation failed" :: Text)])
            Right post -> do
                post <- post |> createRecord
                renderJson post
```

### Using requestBodyJSON

For more control, use `requestBodyJSON` to get the entire parsed JSON body as an Aeson `Value`. This is useful when the JSON structure does not map directly to form-style key-value pairs, or when you want to decode it into a custom type with `FromJSON`:

```haskell
action CreatePostAction = do
    payload <- requestBodyJSON
    -- payload is an Aeson.Value that you can pattern match or decode
    ...
```

If the request body is not valid JSON (or the `Content-Type` is not `application/json`), `requestBodyJSON` will automatically respond with a 400 error -- you do not need to handle that case yourself.

### Using FromJSON for Structured Parsing

For complex payloads, parse the JSON body into a custom type using Aeson's `fromJSON`:

```haskell
import qualified Data.Aeson as Aeson

data CreatePostPayload = CreatePostPayload
    { title :: Text
    , body :: Text
    , tags :: [Text]
    }

instance FromJSON CreatePostPayload where
    parseJSON = withObject "CreatePostPayload" \o -> do
        title <- o .: "title"
        body <- o .: "body"
        tags <- o .:? "tags" .!= []
        pure CreatePostPayload { title, body, tags }

instance Controller PostsController where
    action CreatePostAction = do
        jsonBody <- requestBodyJSON
        case Aeson.fromJSON jsonBody of
            Aeson.Success CreatePostPayload { title, body, tags } -> do
                post <- newRecord @Post
                    |> set #title title
                    |> set #body body
                    |> createRecord
                renderJson post
            Aeson.Error err -> do
                renderJsonWithStatusCode status400 (object ["error" .= err])
```

## Typed Request Bodies and OpenAPI

For new API endpoints, you can opt into typed GADT actions. With this style the
action constructor contains path and query parameters, while a type index
declares the request body. IHP then uses the same body type for JSON decoding,
form decoding, typed controller helpers, typed forms, and OpenAPI docs.

```haskell
data PostInput = PostInput
    { title :: Text
    , body :: Text
    }
    deriving (Generic)

instance FromJSON PostInput
instance ToJSON PostInput
instance ToSchema PostInput

data PostsAction request response where
    UpdatePostAction
        :: { postId :: Id Post
           , returnTo :: Maybe Text
           }
        -> PostsAction ('Body PostInput) ShowView

deriving instance Show (PostsAction request response)
deriving instance Eq (PostsAction request response)

[routes|webRoutes
POST|PATCH /posts/{postId}?returnTo UpdatePostAction
  summary: Update post
  tags: Posts
  success: 200 Successful response
|]
```

The JSON decoder uses `FromJSON`. Form requests use the default generic
`FromFormBody` instance. If an endpoint should only accept JSON, use
`'BodyWith PostInput '[ 'Json]`.

`'Body PostInput` accepts both `application/json` and
`application/x-www-form-urlencoded` by default. If an endpoint should accept only
some encodings, use `BodyWith`:

```haskell
data PostsAction request response where
    CreatePostAction
        :: PostsAction ('BodyWith PostInput '[ 'Json]) ShowView

    UploadPostImageAction
        :: { postId :: Id Post }
        -> PostsAction ('BodyWith PostImageInput '[ 'Multipart]) ShowView
```

Use `'NoBody` when the route has no request body. Path and query parameters are
still constructor fields, so `'NoBody` does not mean "no request data":

```haskell
data PostsAction request response where
    EditPostAction
        :: { postId :: Id Post
           , returnTo :: Maybe Text
           }
        -> PostsAction 'NoBody EditView
```

Inside the action, read the decoded request body with `bodyParam` or copy fields
with `fillBody`. These helpers only compile for fields that exist on the body
type.

```haskell
instance Controller (PostsAction ('Body PostInput) ShowView) where
    type ControllerAction (PostsAction ('Body PostInput) ShowView) =
        ActionDef (PostsAction ('Body PostInput) ShowView) ('Body PostInput) ShowView

    action UpdatePostAction { postId, returnTo } =
        typedAction do
            post <- fetch postId

            post <-
                post
                    |> fillBody @'["title", "body"]
                    |> updateRecord

            pure ShowView { .. }
```

The indented metadata block under the route documents operation metadata such
as summary, tags, status, and descriptions. The request body schema comes from
the action's `BodySpec`; and the response schema is inferred from the returned
view's `JsonView` `JsonResponse` associated type. Use `private` under a typed
route to keep it routable while omitting it from OpenAPI.

```haskell
instance View ShowView where
    html ShowView { post } = [hsx|...|]

instance JsonView ShowView where
    type JsonResponse ShowView = PostResponse

    json ShowView { post } =
        PostResponse
            { id = post.id
            , title = post.title
            , body = post.body
            }
```

Because OpenAPI uses the same route parameter declarations, `BodySpec`, and
typed `JsonView.json` method as runtime routing/rendering, there is no separate
body or response declaration to keep in sync with the handler. With typed GADT
routes, the route type is also the source of truth for runtime parsing,
`pathTo`, and OpenAPI path/query parameter docs, so typed routes do not repeat
path parameters in a separate documentation block.

To serve the generated document and Swagger UI, add the exported Swagger
controller actions to your route DSL:

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

Then include the generated route binding in your front controller:

```haskell
instance FrontController WebApplication where
    controllers =
        webRoutes
            <> openApiRoutes
            <> [ startPage WelcomeAction ]
```

`SwaggerUiAction` uses `pathTo OpenApiJsonAction`, so the Swagger UI always
fetches the OpenAPI JSON from the route you declared.

To customize the generated document metadata or Swagger UI assets, add a
`SwaggerUiControllerConfig` instance. The URL fields are ignored when using the
controller actions because `[routes|...|]` is the source of truth for URLs:

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

## Building a REST API

Here is a complete example of a CRUD API for a `posts` resource. We will assume a `posts` table with `id`, `title`, `body`, and `created_at` columns.

### Types

Define the controller actions in `Web/Types.hs`:

```haskell
data PostsController
    = PostsAction
    | ShowPostAction { postId :: !(Id Post) }
    | CreatePostAction
    | UpdatePostAction { postId :: !(Id Post) }
    | DeletePostAction { postId :: !(Id Post) }
    deriving (Eq, Show, Data)
```

### Controller

Implement the controller in `Web/Controller/Posts.hs`:

```haskell
module Web.Controller.Posts where

import Web.Controller.Prelude
import Network.HTTP.Types (status422)

instance Controller PostsController where
    -- GET /Posts
    action PostsAction = do
        posts <- query @Post |> fetch
        renderJson posts

    -- GET /ShowPost?postId=...
    action ShowPostAction { postId } = do
        post <- fetch postId
        renderJson post

    -- POST /CreatePost
    action CreatePostAction = do
        let post = newRecord @Post
        post
            |> fill @'["title", "body"]
            |> validateField #title nonEmpty
            |> validateField #body nonEmpty
            |> ifValid \case
                Left post -> do
                    renderJsonWithStatusCode status422 (validationErrorsToJson post)
                Right post -> do
                    post <- post |> createRecord
                    renderJson post

    -- POST /UpdatePost?postId=...  or  PATCH /UpdatePost?postId=...
    action UpdatePostAction { postId } = do
        post <- fetch postId
        post
            |> fill @'["title", "body"]
            |> validateField #title nonEmpty
            |> validateField #body nonEmpty
            |> ifValid \case
                Left post -> do
                    renderJsonWithStatusCode status422 (validationErrorsToJson post)
                Right post -> do
                    post <- post |> updateRecord
                    renderJson post

    -- DELETE /DeletePost?postId=...
    action DeletePostAction { postId } = do
        post <- fetch postId
        deleteRecord post
        renderJson (object ["success" .= True])


instance ToJSON Post where
    toJSON post = object
        [ "id" .= post.id
        , "title" .= post.title
        , "body" .= post.body
        , "createdAt" .= post.createdAt
        ]

-- | Convert validation errors on a record to a JSON object
validationErrorsToJson :: (HasField "meta" record MetaBag) => record -> Value
validationErrorsToJson record =
    let MetaBag { annotations } = record.meta
    in object
        [ "errors" .= object (map (\(field, violation) -> (fromString (cs field)) .= violation.message) annotations)
        ]
```

### Testing the API

You can test the API endpoints with `curl`:

```bash
# List all posts
curl http://localhost:8000/Posts

# Show a single post
curl http://localhost:8000/ShowPost?postId=5a8a4c5e-1b5e-4b2a-9c0a-3e5e4b8e1b5e

# Create a post (form-encoded)
curl -X POST http://localhost:8000/CreatePost \
  -d "title=My Post" \
  -d "body=Hello, world!"

# Create a post (JSON)
curl -X POST http://localhost:8000/CreatePost \
  -H "Content-Type: application/json" \
  -d '{"title": "My Post", "body": "Hello, world!"}'

# Update a post
curl -X POST http://localhost:8000/UpdatePost?postId=5a8a4c5e-... \
  -H "Content-Type: application/json" \
  -d '{"title": "Updated Title", "body": "Updated body"}'

# Delete a post
curl -X DELETE http://localhost:8000/DeletePost?postId=5a8a4c5e-...
```

## API Routing

### Using AutoRoute

The simplest approach is to use `AutoRoute`, the same as with HTML controllers. Add to `Web/Routes.hs`:

```haskell
instance AutoRoute PostsController
```

And register the controller in `Web/FrontController.hs`:

```haskell
instance FrontController WebApplication where
    controllers =
        [ startPage WelcomeAction
        , parseRoute @PostsController
        ]
```

This gives you URLs like `/Posts`, `/ShowPost?postId=...`, `/CreatePost`, etc.

### API Controllers Alongside HTML Controllers

You can have API controllers coexist with HTML controllers in the same application. Just give them different names:

```haskell
-- Web/Types.hs

-- HTML controller for browser views
data PostsController
    = PostsAction
    | NewPostAction
    | ShowPostAction { postId :: !(Id Post) }
    | CreatePostAction
    | EditPostAction { postId :: !(Id Post) }
    | UpdatePostAction { postId :: !(Id Post) }
    | DeletePostAction { postId :: !(Id Post) }
    deriving (Eq, Show, Data)

-- JSON API controller
data ApiPostsController
    = ApiPostsAction
    | ApiShowPostAction { postId :: !(Id Post) }
    | ApiCreatePostAction
    | ApiUpdatePostAction { postId :: !(Id Post) }
    | ApiDeletePostAction { postId :: !(Id Post) }
    deriving (Eq, Show, Data)
```

Register both in the FrontController:

```haskell
instance FrontController WebApplication where
    controllers =
        [ parseRoute @PostsController
        , parseRoute @ApiPostsController
        ]
```

### Serving Both HTML and JSON from the Same Action

If you want one action to serve HTML to browsers and JSON to API clients, use `renderHtmlOrJson` with a view that implements both `View` (for HTML) and `JsonView` (for JSON):

```haskell
-- In the controller
action ShowPostAction { postId } = do
    post <- fetch postId
    renderHtmlOrJson ShowView { post }

-- In the view
instance View ShowView where
    html ShowView { post } = [hsx|
        <h1>{post.title}</h1>
        <p>{post.body}</p>
    |]

instance JsonView ShowView where
    json ShowView { post } = toJSON post
```

When a browser requests the page (sending `Accept: text/html`), it gets the HTML response. When an API client requests it with `Accept: application/json`, it gets JSON. You can test this:

```bash
# Get HTML (default)
curl http://localhost:8000/ShowPost?postId=...

# Get JSON
curl -H "Accept: application/json" http://localhost:8000/ShowPost?postId=...
```

### Custom API Routes

For RESTful URLs like `/api/posts` and `/api/posts/:id`, define custom routing with `CanRoute` and `HasPath`:

```haskell
-- Web/Types.hs
data ApiPostsController
    = ApiListPostsAction
    | ApiGetPostAction { postId :: !(Id Post) }
    | ApiCreatePostAction
    | ApiUpdatePostAction { postId :: !(Id Post) }
    | ApiDeletePostAction { postId :: !(Id Post) }
    deriving (Eq, Show, Data)

-- Web/Routes.hs
instance CanRoute ApiPostsController where
    parseRoute' = do
        string "/api/posts"
        let
            listAction = do
                endOfInput
                onlyAllowMethods [GET, HEAD]
                pure ApiListPostsAction

            getAction = do
                string "/"
                postId <- parseId
                endOfInput
                onlyAllowMethods [GET, HEAD]
                pure ApiGetPostAction { postId }

            createAction = do
                endOfInput
                onlyAllowMethods [POST]
                pure ApiCreatePostAction

            updateAction = do
                string "/"
                postId <- parseId
                endOfInput
                onlyAllowMethods [PATCH]
                pure ApiUpdatePostAction { postId }

            deleteAction = do
                string "/"
                postId <- parseId
                endOfInput
                onlyAllowMethods [DELETE]
                pure ApiDeletePostAction { postId }

        createAction <|> updateAction <|> deleteAction <|> getAction <|> listAction

instance HasPath ApiPostsController where
    pathTo ApiListPostsAction = "/api/posts"
    pathTo ApiGetPostAction { postId } = "/api/posts/" <> tshow postId
    pathTo ApiCreatePostAction = "/api/posts"
    pathTo ApiUpdatePostAction { postId } = "/api/posts/" <> tshow postId
    pathTo ApiDeletePostAction { postId } = "/api/posts/" <> tshow postId
```

Note that `createAction` is tried before `getAction` in the alternation. This is because both `/api/posts` (without a trailing id) match the same prefix. Since `createAction` checks for `POST` and `getAction` checks for `GET`/`HEAD`, the parser needs to try the non-id routes first when there is no trailing `/`.

Then register the controller in `Web/FrontController.hs`:

```haskell
instance FrontController WebApplication where
    controllers =
        [ parseRoute @ApiPostsController
        -- ...
        ]
```

Now you have RESTful URLs:

```bash
GET    /api/posts           => ApiListPostsAction
GET    /api/posts/:id       => ApiGetPostAction
POST   /api/posts           => ApiCreatePostAction
PATCH  /api/posts/:id       => ApiUpdatePostAction
DELETE /api/posts/:id       => ApiDeletePostAction
```

### Separate API Application

For larger projects, you can create a separate IHP application for your API (similar to how you might add an `Admin` application). Create an `Api/` directory with its own `Types.hs`, `Routes.hs`, `FrontController.hs`, and controllers. Then mount it in your main `Web/FrontController.hs`:

```haskell
instance FrontController WebApplication where
    controllers =
        [ mountFrontController ApiApplication
        , parseRoute @PostsController
        ]
```

The API controllers will automatically be prefixed with `/api/` (based on the module name `Api`).

## Setting Response Headers

### Using setHeader

Use `setHeader` to add custom headers to the response:

```haskell
action PostsAction = do
    setHeader ("X-Request-Id", "abc-123")
    posts <- query @Post |> fetch
    renderJson posts
```

The `Content-Type` header is automatically set to `application/json` by `renderJson`, so you do not need to set it manually.

### CORS Headers

If your API is called from a different domain (e.g., a JavaScript frontend on a different host), you need to configure CORS. IHP has built-in CORS support via the [wai-cors](https://hackage.haskell.org/package/wai-cors) library.

Configure CORS in `Config/Config.hs`:

```haskell
module Config where

import IHP.Prelude
import IHP.Environment
import IHP.FrameworkConfig
import qualified Network.Wai.Middleware.Cors as Cors

config :: ConfigBuilder
config = do
    option $ Just Cors.simpleCorsResourcePolicy
        { Cors.corsOrigins = Nothing -- Allow all origins. Use Just (["https://example.com"], True) to restrict.
        , Cors.corsMethods = ["GET", "POST", "PUT", "PATCH", "DELETE", "OPTIONS"]
        , Cors.corsRequestHeaders = ["Content-Type", "Authorization", "Accept"]
        }
```

`simpleCorsResourcePolicy` provides sensible defaults. You customize it by overriding individual fields.

To restrict CORS to specific origins:

```haskell
option $ Just Cors.simpleCorsResourcePolicy
    { Cors.corsOrigins = Just (["https://myapp.example.com", "http://localhost:3000"], True)
    , Cors.corsMethods = ["GET", "POST", "PUT", "PATCH", "DELETE", "OPTIONS"]
    , Cors.corsRequestHeaders = ["Content-Type", "Authorization", "Accept"]
    }
```

The second element in the tuple (`True`) enables the `Access-Control-Allow-Credentials` header.

## Error Responses

When building an API, you want error responses in JSON rather than HTML. Here are patterns for common error cases.

### Validation Errors (422)

When record validation fails, return a 422 status with the validation errors:

```haskell
import Network.HTTP.Types (status422)

action CreatePostAction = do
    let post = newRecord @Post
    post
        |> fill @'["title", "body"]
        |> validateField #title nonEmpty
        |> ifValid \case
            Left post -> do
                let MetaBag { annotations } = post.meta
                let errors = object (map (\(field, violation) -> (fromString (cs field)) .= violation.message) annotations)
                renderJsonWithStatusCode status422 (object ["errors" .= errors])
            Right post -> do
                post <- post |> createRecord
                renderJson post
```

### Not Found (404)

Return a JSON 404 response using `renderJsonWithStatusCode`:

```haskell
import Network.HTTP.Types (status404)

renderNotFoundJson :: (?request :: Request) => IO ()
renderNotFoundJson =
    renderJsonWithStatusCode status404 (object ["error" .= ("Not found" :: Text)])
```

Use it in a controller:

```haskell
action ShowPostAction { postId } = do
    postMaybe <- query @Post |> filterWhere (#id, postId) |> fetchOneOrNothing
    case postMaybe of
        Just post -> renderJson post
        Nothing -> renderNotFoundJson
```

### Bad Request (400)

```haskell
import Network.HTTP.Types (status400)

action CreatePostAction = do
    renderJsonWithStatusCode status400 (object ["error" .= ("Invalid request" :: Text)])
```

### Custom Status Codes

Use `renderJsonWithStatusCode` with any HTTP status code from `Network.HTTP.Types`. You will need to import the status codes you use:

```haskell
import Network.HTTP.Types (status201, status403, status404, status422, status500)

-- 201 Created
action CreatePostAction = do
    post <- newRecord @Post
        |> fill @'["title", "body"]
        |> createRecord
    renderJsonWithStatusCode status201 post

-- 403 Forbidden
action ShowSecretAction = do
    renderJsonWithStatusCode status403 (object ["error" .= ("Access denied" :: Text)])
```

### beforeAction for API-Wide Error Handling

Use `beforeAction` to run logic before every action in a controller. This is a good place to verify authentication and return JSON errors:

```haskell
import Network.HTTP.Types (status401)

instance Controller ApiPostsController where
    beforeAction = do
        let apiKey = getHeader "X-API-Key"
        when (apiKey /= Just "secret-key") do
            renderJsonWithStatusCode status401 (object ["error" .= ("Invalid API key" :: Text)])

    action ApiListPostsAction = do
        posts <- query @Post |> fetch
        renderJson posts
    ...
```

When `renderJsonWithStatusCode` (or any render function) is called in `beforeAction`, it short-circuits the request -- the main `action` is never executed.

## Authentication for APIs

### API Key Authentication

A simple approach for service-to-service communication. Check for an API key in a request header:

```haskell
import Network.HTTP.Types (status401)

instance Controller ApiPostsController where
    beforeAction = requireApiKey

    action ApiListPostsAction = do
        posts <- query @Post |> fetch
        renderJson posts

requireApiKey :: (?request :: Request) => IO ()
requireApiKey = do
    let apiKey = getHeader "X-API-Key"
    when (apiKey /= Just expectedApiKey) do
        renderJsonWithStatusCode status401 (object ["error" .= ("Invalid or missing API key" :: Text)])
    where
        expectedApiKey = "your-secret-api-key" -- In practice, load from environment variable
```

For production, load the API key from an environment variable using IHP's config system:

```haskell
-- Config/Config.hs
newtype ApiKey = ApiKey Text

config :: ConfigBuilder
config = do
    apiKey <- ApiKey <$> env @Text "API_KEY"
    option apiKey
```

Then use it in the controller:

```haskell
requireApiKey :: (?context :: ControllerContext, ?request :: Request) => IO ()
requireApiKey = do
    let (ApiKey expectedKey) = getAppConfig @Config.ApiKey
    let providedKey = getHeader "X-API-Key"
    when (providedKey /= Just (cs expectedKey)) do
        renderJsonWithStatusCode status401 (object ["error" .= ("Invalid or missing API key" :: Text)])
```

### Bearer Token Authentication

For user-facing APIs, use bearer tokens in the `Authorization` header:

```haskell
requireBearerToken :: (?request :: Request) => IO ()
requireBearerToken = do
    let authHeader = getHeader "Authorization"
    case authHeader of
        Just header | "Bearer " `isPrefixOf` header -> do
            let token = ByteString.drop 7 header
            -- Validate the token (e.g., look it up in the database)
            tokenValid <- validateToken (cs token)
            unless tokenValid do
                renderJsonWithStatusCode status401 (object ["error" .= ("Invalid token" :: Text)])
        _ -> do
            renderJsonWithStatusCode status401 (object ["error" .= ("Missing Authorization header" :: Text)])
```

### Session-Based Authentication

If your API is consumed by a browser-based SPA on the same domain, you can reuse IHP's built-in session authentication. Since IHP sets an HTTP-only session cookie, AJAX requests from the same origin will automatically include it. See the [Authentication](authentication.html) guide for how to set up session-based authentication.

### HTTP Basic Authentication

For simple cases (e.g., protecting a staging API), use IHP's built-in `basicAuth`:

```haskell
instance Controller ApiPostsController where
    beforeAction = basicAuth "admin" "secret-password" "api"
```

See the [Authentication](authentication.html) guide for more details.
