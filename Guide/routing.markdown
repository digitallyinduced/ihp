# Controller & Actions

```toc
```

## Routing Basics

In your project routes are defined in the `Web/Routes.hs`. Additionally to the defining a route, it also has to be added in `Web/FrontController.hs` to be picked up by the routing system.

The simplest way to define a route is by using `AutoRoute`, which automatically maps each controller action to an url. For a `PostsController`, the definition in `Web/Routes.hs` will look like this:

```haskell
instance AutoRoute PostsController
type instance ModelControllerMap WebApplication Post = PostsController
```

Afterwards enable the routes for `PostsController` in `Web/FrontController.hs` like this:

```haskell
instance FrontController WebApplication where
    controllers = 
        [ -- ...
        , parseRoute @PostsController
        ]
```

Now you can open e.g. `/Posts` to access the `PostsAction`.

## Url Generation

Use `pathTo` to generate a path to a given action:

```haskell
pathTo ShowPostAction { postId = "adddfb12-da34-44ef-a743-797e54ce3786" }
-- /ShowPost?postId=adddfb12-da34-44ef-a743-797e54ce3786
```

To generate a full url, use `urlTo`:

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

The urls are very close to the actual action which is called. Action parameters are taken automatically from the request query. This design helps you to always know which action is actually called, when requesting an url. 

### AutoRoute & Beautiful Urls

Lots of modern browser don't even show the full url bar anymore (e.g. Safari and most mobile browsers). Therefore AutoRoute doesn't aim to generate the "most" beautiful urls out of the box. It's rather optimized for the needs of developers. If you need beautiful urls for SEO reasons, instead of using AutoRoute you can use the more manual APIs of IHP Routing. See the section "[Beautiful URLs](#beautiful-urls)" for details.

### Multiple Parameters

An action constructor can have multiple parameters:

```haskell
data PostsController = EditPostAction { postId :: !(Id Post), userId :: !(Id Post) }
```

This will generate a routing like:

```haskell
GET /EditPost?postId={postId}&userId={userId} => EditPostAction { postId, userId }
```

### Parameter Types

AutoRoute by default only works with UUID arguments (and Id types like `Id Post`). If you have a controller like `data HelloWorldController = HelloAction { name :: Text }` where you have a text parameter, you have to configure AutoRoute like this:

```haskell
instance AutoRoute HelloWorldController where
    parseArgument = parseTextArgument
```

This way the `name` argument is passed as `Text` instead of `UUID`.

Right now AutoRoute supports only a single type for all given parameters. E.g. an action which takes an UUID and a Text is not supported with AutoRoute right now:
```haskell
data HelloController = HelloAction { userId :: !(Id User), name :: Text }
instance AutRoute HelloController -- This will fail at runtime
```

This is a technical problem we hope to fix in the future. Until then consider using `param` for the `Text` parameter.

### Request Methods

When a an action is named a certain way, AutoRoute will pick a certain request method for the route. E.g. for a `DeletePostAction` it will only allow requests with the request method `DELETE` because the action name starts with `Delete`. Here is an overview of all naming patterns and their corresponding request method:

```haskell
Delete_Action => DELETE
Update_Action => POST, PATCH
Create_Action => POST
Show_Action   => GET, HEAD
otherwise     => GET, POST, HEAD
```

If you need more strong rules, consider using the other routing APIs available or overriding the `allowedMethodsForAction` like this:

```haskell
instance AutoRoute HelloWorldController where
    allowedMethodsForAction "HelloAction" = [ GET ]
```

### Application Prefix

When using multiple application in your IHP project, e.g. having an admin backend, AutoRoute will prefix the action urls with the application name. E.g. a controller `HelloWorldController` defined in `Admin/Types.hs` will be automatically prefixed with `/admin` and generate urls such as `/admin/HelloAction`.

This prefixing has special handling for the `Web` module, so that all controllers in the default `Web` module don't have a prefix.

### Advanced: Custom parseArgument

It's possible to use a custom data type as a routing parameter with AutoRoute. Now might be a good point to switch to a custom routing implementation (described later in this Guide) instead of hacking this into AutoRouting. If this warning can't stop you, go ahead.

A `parseArgument` function has the signature:
```haskell
parseArgument :: forall d. Data d => ByteString -> ByteString -> d
```

The first bytestring argument is the field name of the action argument we are dealing with. The second argument is the value of the query string:
```html
MyAction?{firstArgument}={secondArgument}
```

The last argument `d` cannot be implemented in a typesafe way. This is implemented by calling `unsafeCoerce` on our result value before returning it. The result of this is later used with [`fromConstrM`](http://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Data.html#fromConstrM). Therefore misusing `parseArgument` can result in a runtime crash. Again, consider not using this API.

Given we have a custom argument type in the format `ID-{numeric}` like `ID-0`, `ID-1`, etc. We can define a custom `parseCustomIdArgument` like this:

```haskell
import qualified Data.Attoparsec.ByteString.Char8 as Attoparsec
import qualified Data.ByteString.Char8 as ByteString

parseCustomIdArgument :: forall d. Data d => ByteString -> ByteString -> d
parseCustomIdArgument field value =
    value
    |> ByteString.stripPrefix "ID-"
    |> fromMaybe (error "Failed to parse custom id, ID- missing")
    |> Attoparsec.parseOnly (Attoparsec.decimal <* Attoparsec.endOfInput)
    |> \case
        Right value -> unsafeCoerce value
        Left _ -> error "AutoRoute: Failed to parse custom id, numeric part invalid"
```

## Custom Routing

Sometimes you have special needs for your routing. For this case IHP provides a lower-level routing API on which `AutoRoute` is built on.

Let's say we have controller like this:

```haskell
data PostsController = ShowAllMyPostsAction
```

We want requests to `/posts` to map to `ShowAllMyPostsAction`. For that we need to add a `CanRoute` instance:

```haskell
instance CanRoute PostsController where
    parseRoute' = string "/posts" <* endOfInput >> pure ShowAllMyPostsAction
```

The `parseRoute'` function is a parser which reads an url and returns an action of type `PostsController`. The router uses [attoparsec](https://hackage.haskell.org/package/attoparsec). See below for examples on how to use this for building beautiful urls.

Next to the routing itself, we also need implement the url generation:

```haskell
instance HasPath PostsController where
    pathTo ShowAllMyPostsAction = "/posts"
```

### Beautiful URLs

Let's say we want to give our blog post application a beautiful url structure for SEO reasons. Our controller is defined as:

```haskell
data PostsController
    = ShowPostAction { postId :: !(Id Post) }
```

We want our urls to look like this:

```html
/posts/an-example-blog-post
```

Additionally we also want to accept permalinks with the id like this:

```
/posts/f85dc0bc-fc11-4341-a4e3-e047074a7982
```

To accept urls like this, we first need to make some changes to our data structure. We have to make the `postId` optional. Additonally we need to have a parameter for the url slug:

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

Now we define our `CanRoute` instance like this:

```haskell
instance CanRoute PostsController where
    parseRoute' = do
        string "/posts/"
        let postById = do id <- parseId; endOfInput; pure ShowPostAction { postId, slug = Nothing }
        let postBySlug = do slug <- getInput; pure ShowPostAction { postId = Nothing, slug }
        postById <|> postBySlug
```

Additionally we also have to implement the `HasPath` instance:

```haskell
instance HasPath PostsController where
    pathTo ShowPostAction { postId = Just id, slug = Nothing } = "/posts/" <> tshow id
    pathTo ShowPostAction { postId = Nothing, slug = Just slug } = "/posts/" <> slug
```

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

HTML forms don't support special http methods like `DELETE`. To work around this issue, IHP has [a middleware](https://hackage.haskell.org/package/wai-extra-3.0.1/docs/Network-Wai-Middleware-MethodOverridePost.html) which transforms e.g. a `POST` request with a form field `_method` set to `DELETE` to a `DELETE` request.
