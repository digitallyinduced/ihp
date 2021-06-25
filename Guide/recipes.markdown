# Recipes

This section describes best-practice solutions to the common tasks you are facing when building web applications.

```toc

```

## Static Pages

For adding a static page like e.g. a start page, terms of service, privacy, pricing, etc. you usually use a normal controller which just renders the view for that page. The only special thing is, that you might want to customize the routing to have SEO-friendly URLs.

Let's say we have a controller like this defined in `Web.Types`:

```haskell
data StaticController
    = AboutAction
    | TermsAction
    deriving (Eq, Show, Data)
```

The controller implementation will look like this in `Web.Controller.Static`:

```haskell
module Web.Controller.Static where

import Web.Controller.Prelude
import Web.View.Static.Terms
import Web.View.Static.About

instance Controller StaticController where
    action TermsAction = render TermsView
    action AboutAction = render AboutView
```

We can now customize the routing in `Web.Routes` by first deleting the `instance AutoRoute StaticController` statement to delete the auto generated routing configuration and append:

```haskell
instance HasPath StaticController where
    pathTo TermsAction = "/terms"
    pathTo AboutAction = "/about"

instance CanRoute StaticController where
    parseRoute' =
        (string "/terms" <* endOfInput >> pure TermsAction)
        <|> (string "/about" <* endOfInput >> pure AboutAction)
```

Now the terms can be reached at `/terms` instead of `/Terms`. The about is at `/about` now, instead of `/About`.

## Uploading a user profile picture

You can easily upload a user profile picture using `uploadImageWithOptions` inside your `UpdateUserAction`:

```haskell
action UpdateUserAction { userId } = do
    user <- fetch userId
    accessDeniedUnless (userId == currentUserId)

    let profilePictureOptions = ImageUploadOptions
            { convertTo = "jpg"
            , imageMagickOptions = "-resize '1024x1024^' -gravity north -extent 1024x1024 -quality 85% -strip"
            }

    user
        |> fill @["firstname", "lastname", "pictureUrl"]
        |> uploadImageWithOptions profilePictureOptions #pictureUrl
        >>= ifValid \case
            Left user -> render EditView { .. }
            Right user -> do
                user <- user |> updateRecord
                setSuccessMessage "Your changes have been saved."
                redirectTo EditUserAction { .. }
```

This accepts any kind of image file compatible with ImageMagick, resize it, reduce the image quality, strip all meta information, and save it as JPG. The file is stored inside the `static/uploads` folder in the project (the directory will be created if it does not exist).

In your view, just use the image URL like `<img src={get #pictureUrl currentUser}/>`.
Note that when you define the `picture_url` field in your `users` table that you
must check the `Nullable` select box with a default `Null`. This ensures your
`pictureUrl` data has a `Maybe Text` type and can handle
cases where the user has not uploaded any image.

If ImageMagick is not installed you will get a `picture.upload` in the uploads folder, but no `picture.jpg`. To install ImageMagick, checkout [Using a native dependency](package-management.html#using-a-native-dependency).

There is currently no special form helper for file uploads. Just specify it manually like this:

```haskell
instance View EditView where
    html EditView { .. } = [hsx|
        <h1>Profil bearbeiten</h1>

        {renderForm user}
    |]
        where
            picturePath :: Text
            picturePath = get #pictureUrl user

            renderForm :: User -> Html
            renderForm user = formFor user [hsx|
                <div>
                    <h5>
                        Profilfoto
                    </h5>

                    <div style="max-width: 300px">
                        <div class="form-group">
                            <label for="user_picture_url">
                                <img id="user_picture_url_preview" src={picturePath} style="width: 12rem; min-height: 12rem; min-width: 12rem" class="mt-2 img-thumbnail text-center text-muted" alt="Foto auswählen"/>
                                <input id="user_picture_url" type="file" name="pictureUrl" class="form-control form-control-file" style="display: none" data-preview="#user_picture_url_preview"/>
                                <a class="d-block text-muted text-center" href="#" onclick="document.getElementById('user_picture_url_preview').click()">Neues Foto auswählen</a>
                            </label>
                        </div>
                    </div>
                </div>

                {(textField #firstname) { fieldLabel = "Vorname:" }}
                {(textField #lastname) { fieldLabel = "Nachname:" }}
                {submitButton { label = "Speichern" }}
            |]
```

## Checking that the current user has permission to access the action

Use [accessDeniedUnless](https://ihp.digitallyinduced.com/api-docs/IHP-LoginSupport-Helper-Controller.html#v:accessDeniedUnless) like this:

```haskell
action EditPostAction { postId } = do
    post <- fetch postId
    accessDeniedUnless (get #authorId post == currentUserId)

    renderHtml EditView { .. }
```

## Creating a custom validator

If needed you can just write your constraint, e.g. like this:

```haskell
nonEmpty :: Text -> ValidatorResult
nonEmpty "" = Failure "This field cannot be empty"
nonEmpty _ = Success

isAge :: Int -> ValidatorResult
isAge = isInRange (0, 100)
```

This is also useful if you need the messages to be in another language.

## Checking that an email is unique

Use [`validateIsUnique`](https://ihp.digitallyinduced.com/api-docs/IHP-ValidationSupport-ValidateIsUnique.html#v:validateIsUnique).

## Don't auto-open the app in the browser

To prevent the IHP development server from automatically opening the development tooling in your web browser when running `./start`, set the `IHP_BROWSER` environment variable to `echo`:

```bash
export IHP_BROWSER=echo
./start
```

This will then just print out the URL which would be opened on start.

## Getting an `Id Something` from a `UUID`

Sometimes you have a UUID value that represents some record id. To get the types right, you can transform it like this:

```haskell
let myUUID = ...
let projectId = (Id myUUID) :: Id Project
```

In case the id is hard coded, you can just type UUID value with the right type signature like this:

```haskell
let projectId = "ca63aace-af4b-4e6c-bcfa-76ca061dbdc6" :: Id Project
```

## Getting a `Id Something` from a `Text` / `ByteString` / `String`

Sometimes you have a text, bytestring, or string which represents some record id. You can transform it into an Id like this:

```haskell
let myUUID :: Text = ...
let projectId = textToId myUUID
```

In case the id is hard coded, you can just type UUID value with the right type signature like this:

```haskell
let projectId = "ca63aace-af4b-4e6c-bcfa-76ca061dbdc6" :: Id Project
```

## Having an image as a Logout button

The `DeleteSessionAction` expects a `HTTP DELETE` request, which is set by JavaScript on click. This does not currently work well with an image inside a link. A workaround is to have the image be the background, like this:

```html
<a
    href="{DeleteSessionAction}"
    class="js-delete js-delete-no-confirm"
    style="background:url(/logout.svg) left center no-repeat;width:40px"
></a>
```

## Making a dynamic Login/Logout button

Depending on the `Maybe User` type in the [ControllerContext](https://ihp.digitallyinduced.com/api-docs/IHP-Controller-Context.html), by using `fromFrozenContext` we can tell if no user logged in when the `Maybe User` is `Nothing`, and confirm someone is logged in if the `Maybe User` is a `Just user`. Here is an example of a navbar, which has a dynamic Login/Logout button. You can define this in your View/Layout to reuse this in your Views.

> The `@` syntax from `fromFrozenContext @(Maybe User)` is just syntax sugar for `let maybeUser :: Maybe User = fromFrozenContext`

```haskell
navbar :: Html
navbar = [hsx|
<nav class="navbar navbar-expand-lg navbar-light bg-light">
  <a class="navbar-brand" href="#">IHP Blog</a>
  <button class="navbar-toggler" type="button" data-toggle="collapse" data-target="#navbarSupportedContent" aria-controls="navbarSupportedContent" aria-expanded="false" aria-label="Toggle navigation">
    <span class="navbar-toggler-icon"></span>
  </button>

  <div class="collapse navbar-collapse" id="navbarSupportedContent">
    <ul class="navbar-nav mr-auto">
      <li class="nav-item">
        <a class="nav-link" href={PostsAction}>Posts</a>
      </li>
    </ul>
    {loginLogoutButton}
  </div>
</nav>
|]
    where
        loginLogoutButton :: Html
        loginLogoutButton =
            case fromFrozenContext @(Maybe User) of
                Just user -> [hsx|<a class="js-delete js-delete-no-confirm text-secondary" href={DeleteSessionAction}>Logout</a>|]
                Nothing -> [hsx|<a class="text-secondary" href={NewSessionAction}>Login</a>|]
```

You can see this code in action in the [`auth` branch from our example blog](https://github.com/digitallyinduced/ihp-blog-example-app/blob/auth/Web/View/Layout.hs).

Protip: If the `Maybe User` is a `Just user` you can use the user object to run specific actions or retrieve information from it. This way you could display the username of the logged-in user above the logout button.

## Making an HTTP request

To make an HTTP request, you need `Wreq`. You need to add it to your Haskell dependencies in the `default.nix` file, like here:

```bash
...
haskellDeps = p: with p; [
    cabal-install
    base
    wai
    text
    hlint
    p.ihp
    wreq <-- Add this
];
...
```

Then you need to import it in your controller/script:

```haskell
import qualified Network.Wreq as Wreq
```

To simply fetch and render another website, you could use a function like this:

```haskell
handleFetchAction :: _ => Text -> _
handleFetchAction url = do
    documentBody <- do
        response <- Wreq.get (cs url)
        pure (response ^. Wreq.responseBody)
    renderPlain (cs documentBody)
```

When using `handleFetchAction "https://google.com/"`, your app would display the google homepage.

## Confirm before the link is used

To confirm before a link is fired add an `onclick` to the link.

```haskell
[hsx|
    <a href={UsersAction} onclick="if (!confirm('Do you really want to delete the internet?')) event.preventDefault();"></a>
|]
```

## How to generate a random string

To generate a random string which can be used as a secure token or hash use `generateAuthenticationToken`:

```haskell
import IHP.AuthSupport.Authentication -- Not needed if you're inside a IHP controller

do
    token <- generateAuthenticationToken
    -- token = "11D3OAbUfL0P9KNJ09VcUfCO0S9RwI"
```

## Working with Dates in IHP and Haskell

https://zacwood.me/posts/2020-12-29-dates-ihp/

## Basic Data Scraping, HTTP and JSON

https://zacwood.me/posts/haskell-scraping-1/

## Custom Configuration / Dealing with Secrets

Sometimes you want to have a custom configuration flag inside your application. The simplest way is to just declare a custom variable in `Config/Config.hs` like this:

```haskell
stripePublicKey :: Text
stripePublicKey = "..."
```

Then you can use it by importing the Config module at the call sites:

```haskell
import qualified Config

action MyAction = do
    let stripePublicKey = Config.stripePublicKey
```

## Custom Configuration with IO

The easiest way to add custom configuration to IHP apps is to define a static string like this:

```haskell
stripePublicKey :: Text
stripePublicKey = "..."
```

When this doesn't work, you can run any custom code to load your configuration and make it available application-wide.

In this example we're going to provide a custom Redis connection URL to our app. We read a string from the `REDIS_URL` env variable on app startup and then access it using `?applicationContext` from inside our controller actions:

Inside our `Config.hs` we need to read the `REDIS_URL` env var:

```haskell
newtype RedisUrl = RedisUrl String -- A wrapper type for our string. This is required by IHP.

config :: ConfigBuilder
config = do
    option Development
    option (AppHostname "localhost")
    
    redisUrl <- liftIO $ System.Environment.getEnv "REDIS_URL"
    option (RedisUrl redisUrl
```

From our actions, scripts and job workers we can access it like this:

```haskell
import qualified Data.TMap as TMap
import Config -- For importing the RedisUrl data type

action MyAction = do
    let (RedisUrl connectionString) = redisUrl
    putStrLn ("REDIS_URL = " <> cs connectionString)

redisUrl :: (?context :: ControllerContext) => RedisUrl
redisUrl = ?context
        |> getFrameworkConfig
        |> get #appConfig
        |> TMap.lookup @RedisUrl
        |> fromMaybe (error "Could not find RedisUrl in config")
```

## Read a file from filesystem / create a custom 404 page

If for some reason https://ihp.digitallyinduced.com/Guide/routing.html#custom-404-page 
is not easy enough - you can also write your own function which reads a file and responses
with a 404:


```haskell

    import qualified Data.ByteString.Lazy as LBS
    customNotFoundResponse :: IO ()
    customNotFoundResponse = do
    page <- LBS.readFile "static/404.html"
    respondAndExit $ responseLBS status404 [(hContentType, "text/html")] page

```

Now you can use your customNotFoundResponse:

```haskell

    action WelcomeAction  = do
        post <- fetch ("30a73014-101e-4269-be91-be6c019de289" :: Id Post) 
        case post of
            Nothing ->  customNotFoundResponse -- Database record disappeared !!!
            Just post -> do
                    render ShowView { .. }

```
