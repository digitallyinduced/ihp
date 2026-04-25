# Routing

```toc

```

## Routing Basics

In your project, routes are defined in the `Web/Routes.hs`. In addition to defining that route, it also has to be added in `Web/FrontController.hs` to be picked up by the routing system.

IHP offers two ways to wire URLs to actions. New projects should pick the **explicit routes DSL** — it makes the URL ↔ action mapping visible at the route site. The legacy **AutoRoute** approach derives URLs automatically from constructor names and is still fully supported for existing apps.

### Option 1 — The `[routes|…|]` DSL (recommended for new apps)

Declare each route explicitly in `Web/Routes.hs`:

```haskell
[routes|webRoutes
GET    /Posts                    PostsAction
GET    /NewPost                  NewPostAction
POST   /CreatePost               CreatePostAction
GET    /ShowPost?postId          ShowPostAction
GET    /EditPost?postId          EditPostAction
POST   /UpdatePost?postId        UpdatePostAction
DELETE /DeletePost?postId        DeletePostAction
|]
```

Then splat the generated `webRoutes` binding into your `FrontController`:

```haskell
instance FrontController WebApplication where
    controllers = webRoutes
```

See [Explicit Routes DSL](#explicit-routes-dsl) below for the full syntax.

### Option 2 — `AutoRoute` (legacy)

`AutoRoute` derives URLs from your action ADT without any explicit spec:

```haskell
instance AutoRoute PostsController
```

Enable the routes for `PostsController` in `Web/FrontController.hs`:

```haskell
instance FrontController WebApplication where
    controllers =
        [ -- ...
        , parseRoute @PostsController
        ]
```

Now you can open e.g. `/Posts` to access the `PostsAction`.

## Explicit Routes DSL

The `[routes|…|]` quasi-quoter declares each URL explicitly. The quoter reifies the action ADT at compile time, so field types for path captures and query parameters come from the record definition — no runtime reflection, no `deriving Data`.

### A first example

```haskell
-- Web/Types.hs
data PostsController
    = PostsAction
    | NewPostAction
    | ShowPostAction { postId :: !(Id Post) }
    | CreatePostAction
    deriving (Eq, Show)
```

```haskell
-- Web/Routes.hs
[routes|PostsController
GET    /Posts             PostsAction
GET    /NewPost           NewPostAction
GET    /ShowPost?postId   ShowPostAction
POST   /CreatePost        CreatePostAction
|]
```

Each line is `METHOD path actionName`. Path captures use `{name}` (RFC 6570). Query params use `?name1&name2` after the path. The record field types decide how captures and query params are parsed — a `postId :: Id Post` field parses as a UUID, a `page :: Maybe Int` is an optional integer, and so on.

### Path captures

Bind a URL segment to a record field with `{name}`:

```haskell
data PostsController = ShowPostAction { postId :: !(Id Post) }

[routes|PostsController
GET /posts/{postId}    ShowPostAction
|]
```

`pathTo (ShowPostAction "123e4567-e89b-12d3-a456-426614174000")` renders `/posts/123e4567-e89b-12d3-a456-426614174000`.

Splat captures (the rest of the path) use `{+name}`, also following RFC 6570:

```haskell
data FilesController = DownloadAction { path :: Text }

[routes|FilesController
GET /files/{+path}     DownloadAction
|]
```

The `path` field is decoded as `Text` and captures everything after `/files/`, including `/` characters.

### Query parameters

After the path, declare query params with a `?name&name` suffix:

```haskell
data PostsController
    = SearchAction { q :: Text, page :: Maybe Int, tags :: [Text] }
    | ShowPostAction { postId :: !(Id Post) }

[routes|PostsController
GET /search?q&page&tags       SearchAction
GET /ShowPost?postId          ShowPostAction
|]
```

Field type drives the URL shape:

- **Required** (`a`): missing or unparseable values respond `404`.
- **Optional** (`Maybe a`): absent or unparseable values decode to `Nothing`; `pathTo` omits the param when the value is `Nothing`.
- **List** (`[a]`): collected from every matching `?k=v` repetition; `pathTo` emits one `k=v` pair per element; an empty list omits the field.

Every record field of the action constructor must be covered by either a path capture or a query-param entry. Leftover fields fail at splice time with a pointer to the exact fields not yet bound.

### Rename a field

To map a URL-side name to a differently named record field, use `{ field = #captureName }` after the action. Works for path captures and query params alike:

```haskell
-- capture name in the URL is `id`, record field is `postId`
GET /ShowPost?id            ShowPostAction { postId = #id }

-- path-capture rename
GET /orgs/{org}/users/{user}   ShowMemberAction { organizationId = #org, userId = #user }
```

### Methods and `ANY`

Each route starts with one or more HTTP methods separated by `|`:

```haskell
GET|POST /api/widgets    WidgetsEndpointAction
```

`ANY` expands to all methods:

```haskell
ANY /api/echo            EchoAction
```

`GET` automatically accepts `HEAD` as well — `HEAD /foo` won't return `405` when the route declares `GET /foo`.

### Header forms

The line above the first route is the header. It takes three shapes:

1. **Uppercase identifier** — a single controller type. The splice reifies that type and emits `HasPath` + `CanRoute` instances for it.

   ```haskell
   [routes|PostsController
   GET /posts          PostsAction
   |]
   ```

2. **Lowercase identifier** — a binding name for a multi-controller block. The splice still emits `HasPath` + `CanRoute` per referenced type, plus a top-level `webRoutes :: [ControllerRoute app]` binding that you can splat into `FrontController.controllers`.

   ```haskell
   [routes|webRoutes
   GET /posts          PostsAction
   GET /users          UsersAction
   |]

   instance FrontController WebApplication where
       controllers = webRoutes
   ```

3. **Omitted** — header-less. Splice emits instances only; no binding.

### Compile-time validation

The splice runs several checks on every `[routes|…|]` block and fails at compile time — pointing at the DSL line number — if any of the following go wrong:

- A path capture references an unknown field
- A `?name` query parameter references an unknown field
- A field appears in both the path and the query list
- An action constructor has a record field not covered by the route
- A query parameter is declared twice
- The DSL syntax itself is malformed (unknown method, missing path, etc.)

The error messages include the DSL line number and the list of known fields, so fixing them is usually a one-line change.

### Mixing with AutoRoute

The DSL and AutoRoute can coexist in the same application. One controller using `instance AutoRoute` and another using `[routes|…|]` is a supported configuration — both compile into the same underlying route trie at startup.



## Changing the Start Page / Home Page

You can define a custom start page action using the [`startPage`](https://ihp.digitallyinduced.com/api-docs/IHP-RouterSupport.html#v:startPage) function like this:

```haskell
instance FrontController WebApplication where
    controllers =
        [ startPage ProjectsAction
        -- Generator Marker
        ]
```

In a new IHP project, you usually have a [`startPage WelcomeAction`](https://ihp.digitallyinduced.com/api-docs/IHP-RouterSupport.html#v:startPage) defined. Make sure to remove this line. Otherwise, you will still see the default IHP welcome page.

**Note:** The `WelcomeAction` controller is provided by the separate `ihp-welcome` package, which is typically only used in new projects for the initial boilerplate.

## URL Generation

Use [`pathTo`](https://ihp.digitallyinduced.com/api-docs/IHP-ViewPrelude.html#v:pathTo) to generate a path to a given action:

```haskell
pathTo ShowPostAction { postId = "adddfb12-da34-44ef-a743-797e54ce3786" }
-- /ShowPost?postId=adddfb12-da34-44ef-a743-797e54ce3786
```

To generate a full URL, use [`urlTo`](https://ihp.digitallyinduced.com/api-docs/IHP-ViewPrelude.html#v:urlTo):

```haskell
urlTo NewUserAction
-- http://localhost:8000/NewUser
```

## AutoRoute

Let's say our `PostsController` is defined in `Web/Types.hs` like this:

```haskell
data PostsController
    = PostsAction
    | NewPostAction
    | ShowPostAction { postId :: !(Id Post) }
    | CreatePostAction
    | EditPostAction { postId :: !(Id Post) }
    | UpdatePostAction { postId :: !(Id Post) }
    | DeletePostAction { postId :: !(Id Post) }
```

Using `instance AutoRoute PostsController` will give us the following routing:

```haskell
GET /Posts                          => PostsAction
GET /NewPost                        => NewPostAction
GET /ShowPost?postId={postId}       => ShowPostAction { postId }
POST /CreatePost                    => CreatePostAction
GET /EditPost?postId={postId}       => EditPostAction { postId }
POST /UpdatePost?postId={postId}    => UpdatePostAction { postId }
PATCH /UpdatePost?postId={postId}   => UpdatePostAction { postId }
DELETE /DeletePost?postId={postId}  => DeletePostAction { postId }
```

The URLs are very close to the actual action which is called. Action parameters are taken automatically from the request query. This design helps you to always know which action is called when requesting an URL.

### AutoRoute & Beautiful URLs

Lots of modern browsers don't even show the full URL bar anymore (e.g. Safari and most mobile browsers). Therefore AutoRoute doesn't aim to generate the "most" beautiful URLs out of the box. It's rather optimized for the needs of developers. If you need beautiful URLs for SEO reasons, instead of using AutoRoute you can use the more manual APIs of IHP Routing. See the section "[Beautiful URLs](#beautiful-urls)" for details.

### Multiple Parameters

An action constructor can have multiple parameters:

```haskell
data PostsController = EditPostAction { postId :: !(Id Post), userId :: !(Id User) }
```

This will generate a routing like:

```haskell
GET /EditPost?postId={postId}&userId={userId} => EditPostAction { postId, userId }
```

### Parameter Types

AutoRoute works with the following parameter types:

- `Text`
- `[Text]`
- `Maybe Text`
- `Int`
- `[Int]`
- `Maybe Int`
- `Id` (for all model types)

If a Maybe value is `Nothing`, the value will be left out of the query parameter. Otherwise it will be included with the value:

```haskell
data MyController = DefaultAction { maybeParam :: Maybe Text }

pathTo (MyController Nothing) ==> "/Default"
pathTo (MyController "hello") ==> "/Default?maybeParam=hello"
```

List values are represented as comma separated lists. If the parameter is not present, the list will default to the empty list:


```haskell
data MyController = DefaultAction { listParam :: Maybe [Int] }

pathTo (MyController []) ==> "/Default"
pathTo (MyController [1,2,3]) ==> "/Default?listParam=1,2,3"
```

## For Integer ID types

AutoRoute needs some help if your model does not use UUID as the id type and uses an integer based type instead. To get this to work, add the following to the
[`AutoRoute`](https://ihp.digitallyinduced.com/api-docs/IHP-RouterSupport.html#t:AutoRoute) instance declarations for each controller that needs to parse an integer ID type as an argument:

```haskell
instance AutoRoute TestController where
    autoRoute = autoRouteWithIdType (parseIntegerId @(Id ModelType))
```

### Request Methods

When an action is named a certain way, AutoRoute will pick a certain request method for the route. E.g. for a `DeletePostAction` it will only allow requests with the request method `DELETE` because the action name starts with `Delete`. Here is an overview of all naming patterns and their corresponding request method:

```haskell
Delete_Action => DELETE
Update_Action => POST, PATCH
Create_Action => POST
Show_Action   => GET, HEAD
otherwise     => GET, POST, HEAD
```

If you need more strong rules, consider using the other routing APIs available or overriding the [`allowedMethodsForAction`](https://ihp.digitallyinduced.com/api-docs/IHP-RouterSupport.html#v:allowedMethodsForAction) like this:

```haskell
instance AutoRoute HelloWorldController where
    allowedMethodsForAction "HelloAction" = [ GET ]
```

### Application Prefix

When using multiple applications in your IHP project, e.g. having an admin back-end, AutoRoute will prefix the action URLs with the application name. E.g. a controller `HelloWorldController` defined in `Admin/Types.hs` will be automatically prefixed with `/admin` and generate URLs such as `/admin/HelloAction`.

This prefixing has special handling for the `Web` module so that all controllers in the default `Web` module don't have a prefix.

## Overriding Individual AutoRoute Actions

Sometimes you want a custom URL for just one or two actions, but the default AutoRoute URLs are fine for the rest. Instead of manually implementing `CanRoute` and `HasPath` for every action, you can override individual actions using `customRoutes` and `customPathTo`:

```haskell
data PostsController
    = PostsAction
    | NewPostAction
    | ShowPostAction { postId :: !(Id Post) }
    | CreatePostAction
    | EditPostAction { postId :: !(Id Post) }
    | UpdatePostAction { postId :: !(Id Post) }
    | DeletePostAction { postId :: !(Id Post) }

instance AutoRoute PostsController where
    customRoutes = do
        string "/posts/"
        postId <- parseId
        endOfInput
        onlyAllowMethods [GET, HEAD]
        pure ShowPostAction { postId }

    customPathTo ShowPostAction { postId } = Just ("/posts/" <> tshow postId)
    customPathTo _ = Nothing
```

With this setup:

- `ShowPostAction` is accessible at `/posts/{postId}` (the custom URL)
- `ShowPostAction` is also still accessible at `/ShowPost?postId={postId}` (the auto-generated URL, as a fallback)
- All other actions (`PostsAction`, `NewPostAction`, `CreatePostAction`, etc.) keep their auto-generated routes unchanged
- `pathTo ShowPostAction { postId }` generates `/posts/{postId}` (the custom URL)
- `pathTo PostsAction` generates `/Posts` (the auto-generated URL as usual)

The `customRoutes` parser is tried first, before the auto-generated routes. If it doesn't match, the auto-generated routes are tried as usual. Return `Nothing` from `customPathTo` for any action that should use the default URL generation.

## Custom Routing

Sometimes you have special needs for your routing. For this case, IHP provides a lower-level routing API on which [`AutoRoute`](https://ihp.digitallyinduced.com/api-docs/IHP-RouterSupport.html#t:AutoRoute) is built.

Let's say we have a controller like this:

```haskell
data PostsController = ShowAllMyPostsAction
```

We want requests to `/posts` to map to `ShowAllMyPostsAction`. For that we need to add a [`CanRoute`](https://ihp.digitallyinduced.com/api-docs/IHP-RouterSupport.html#t:CanRoute) instance:

```haskell
instance CanRoute PostsController where
    parseRoute' = string "/posts" <* endOfInput >> pure ShowAllMyPostsAction
```

The [`parseRoute'`](https://ihp.digitallyinduced.com/api-docs/IHP-RouterSupport.html#v:parseRoute-39-) function is a parser that reads an URL and returns an action of type `PostsController`. The router uses [attoparsec](https://hackage.haskell.org/package/attoparsec). See below for examples on how to use this for building beautiful URLs.

Next to the routing itself, we also need to implement the URL generation:

```haskell
instance HasPath PostsController where
    pathTo ShowAllMyPostsAction = "/posts"
```

### Beautiful URLs

Let's say we want to give our blog post application a beautiful URL structure for SEO reasons. Our controller is defined as:

```haskell
data PostsController
    = ShowPostAction { postId :: !(Id Post) }
```

We want our URLs to look like this:

```html
/posts/an-example-blog-post
```

Additionally we also want to accept permalinks with the id like this:

```
/posts/f85dc0bc-fc11-4341-a4e3-e047074a7982
```

To accept URLs like this, we first need to make some changes to our data structure. We have to make the `postId` optional. Additionally, we need to have a parameter for the URL slug:

```haskell
data PostsController
    = ShowPostAction { postId :: !(Maybe (Id Post)), slug :: !(Maybe Text) }
```

This will also require us to make changes to our action implementation:

```haskell
action ShowPostAction { postId, slug } = do
    post <- case slug of
            Just slug -> query @Post |> filterWhere (#slug, slug) |> fetchOne
            Nothing   -> fetchOne postId
    -- ...
```

This expects the `posts` table to have a field `slug :: Text`.

Now we define our [`CanRoute`](https://ihp.digitallyinduced.com/api-docs/IHP-RouterSupport.html#t:CanRoute) instance like this:

```haskell
instance CanRoute PostsController where
    parseRoute' = do
        string "/posts/"
        let postById = do id <- parseId; endOfInput; pure ShowPostAction { postId = Just id, slug = Nothing }
        let postBySlug = do slug <- remainingText; pure ShowPostAction { postId = Nothing, slug = Just slug }
        postById <|> postBySlug
```

Additionally we also have to implement the [`HasPath`](https://ihp.digitallyinduced.com/api-docs/IHP-RouterSupport.html#t:HasPath) instance:

```haskell
instance HasPath PostsController where
    pathTo ShowPostAction { postId = Just id, slug = Nothing } = "/posts/" <> tshow id
    pathTo ShowPostAction { postId = Nothing, slug = Just slug } = "/posts/" <> slug
```

### Helper Functions

The [`IHP.RouterSupport`](https://ihp.digitallyinduced.com/api-docs/IHP-RouterSupport.html) module includes helpers functions such as:

-   [`parseUUID`](https://ihp.digitallyinduced.com/api-docs/IHP-RouterSupport.html#v:parseUUID) to parse and return an UUID

-   [`parseId`](https://ihp.digitallyinduced.com/api-docs/IHP-RouterSupport.html#v:parseId) to parse an UUID, afterwards wraps it in an Id

-   [`parseText`](https://ihp.digitallyinduced.com/api-docs/IHP-RouterSupport.html#v:parseText) to parse until the next `/` character

-   [`routeParam`](https://ihp.digitallyinduced.com/api-docs/IHP-RouterSupport.html#v:routeParam) to parse route query parameters

### Real-World Example

Here is a real world example of a custom routing implementation for a custom Apple Web Service interface implemented at digitally induced:

```haskell
instance CanRoute RegistrationsController where
    parseRoute' = do
        appleDeviceId <- string "AppleWebService/v1/devices/" *> parseText <* "/registrations/"
        passType <- parseText

        let create = do
            string "/"
            memberId  <- parseId
            endOfInput
            pure CreateRegistrationAction { .. }
        let show = do
            endOfInput
            pure ShowRegistrationAction { .. }

        choice [ create, show ]

instance HasPath RegistrationsController where
    pathTo CreateRegistrationAction { appleDeviceId, memberId } = "/AppleWebService/v1/devices/" <> appleDeviceId <> "/registrations/" <> passType <> "/" <> tshow memberId
    pathTo ShowRegistrationAction { appleDeviceId } = "/AppleWebService/v1/devices/" <> appleDeviceId <> "/registrations/" <> passType
```

## Method Override Middleware

HTML forms only support GET and POST methods, but IHP's router expects DELETE requests for delete actions (and PUT/PATCH for updates). To bridge this gap, IHP includes [a middleware](https://hackage.haskell.org/package/wai-extra-3.0.1/docs/Network-Wai-Middleware-MethodOverridePost.html) that transforms a POST request with a hidden form field `_method` into the corresponding HTTP method.

For example, this form sends a DELETE request:

```haskell
<form method="POST" action={DeleteWidgetAction widget.id}>
    <input type="hidden" name="_method" value="DELETE"/>
    <button type="submit">Delete</button>
</form>
```

This is important because actions with side effects (creating, updating, deleting data) should never use GET requests. Plain `<a>` links make GET requests, so they are not suitable for side-effect actions. See the [Actions with Side Effects](form.html#actions-with-side-effects) section in the Forms guide for a full explanation and examples.

## Custom 403 and 404 pages

You can override the default 403 access denied and the default 404 not found pagesby creating a new file at `static/403.html` and `static/404.html`. Then IHP will render that HTML file instead of displaying the default IHP page.
