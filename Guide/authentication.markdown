# Authentication

```toc
```

## Quick-and-Dirty: HTTP Basic Auth

While IHP provides an authentication toolkit out of the box, it also provides a shortcut for cases where you just want the simplest possible way to enforce a hard-coded username/password before accessing your new web application. This shortcut leverages HTTP Basic Authentication built into all browsers:

```haskell
instance Controller WidgetsController where
    beforeAction = basicAuth "sanja" "hunter2" "myapp"
```

The parameters are: username, password, and authentication realm. The realm can be thought of as an area of validity for the credentials. It is common to put the project name, but it can also be blank (meaning the entire domain).

## Introduction - Real Authentication

The usual convention in IHP is to call your user record `User`. When there is an admin user, we usually call the record `Admin`. In general, the authentication can work with any kind of record. The only requirement is that it has an id field.

To use the authentication module, your `users` table needs to have at least an `id`, `email`, `password_hash`, `locked_at` and `failed_login_attempts` field. Add this to `Schema.sql`:

```sql
CREATE TABLE users (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    email TEXT NOT NULL,
    password_hash TEXT NOT NULL,
    locked_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    failed_login_attempts INT DEFAULT 0 NOT NULL
);
```

The password authentication saves the passwords as a salted hash using the [pwstore-fast library](https://hackage.haskell.org/package/pwstore-fast-2.4.4/docs/Crypto-PasswordStore.html). By default, a user will be locked for an hour after 10 failed login attempts.

### Aside: Admin authentication

If you are creating an admin sub-application, first use the code generator to create an application called `Admin`, then follow this guide replacing `Web` with `Admin` and `User` with `Admin` everywhere (except for the lower-case `user` in the file `Admin/View/Sessions/New.hs`, which comes from an imported module).

## Setup

Currently, the authentication toolkit has to be enabled manually. We plan to do this setup using a code generator in the future.

#### Controller Helpers

First, we need to enable the controller helpers. Open `Application/Helper/Controller.hs`. It will look like this:

```haskell
module Application.Helper.Controller (
    -- To use the built in login:
    -- module IHP.LoginSupport.Helper.Controller
) where

-- Here you can add functions which are available in all your controllers

-- To use the built in login:
-- import IHP.LoginSupport.Helper.Controller
```

Enable the import and re-export for `IHP.LoginSupport.Helper.Controller` and add a type instance `type instance CurrentUserRecord = User`:

```haskell
module Application.Helper.Controller (
    module IHP.LoginSupport.Helper.Controller
) where

-- Here you can add functions which are available in all your controllers

import IHP.LoginSupport.Helper.Controller
import Generated.Types

type instance CurrentUserRecord = User
```

#### View Helpers

Additionally, we also need to enable the view helpers. Open `Application/Helper/View.hs`. It will look like this:

```haskell
module Application.Helper.View (
    -- To use the built in login:
    -- module IHP.LoginSupport.Helper.View
) where

-- Here you can add functions which are available in all your views

-- To use the built in login:
-- import IHP.LoginSupport.Helper.View
```

Enable the import and re-export for `IHP.LoginSupport.Helper.View`:

```haskell
module Application.Helper.View (
    module IHP.LoginSupport.Helper.View
) where

-- Here you can add functions which are available in all your views

import IHP.LoginSupport.Helper.View
```

#### Activating the Session

Open `Web/FrontController.hs`. Add an import for `IHP.LoginSupport.Middleware` and `Web.Controller.Sessions`:

```haskell
import IHP.LoginSupport.Middleware
import Web.Controller.Sessions
```

We then need to mount our session controller by adding `parseRoute @SessionController`:

```haskell
instance FrontController WebApplication where
    controllers =
        [ startPage WelcomeAction
        , parseRoute @SessionsController -- <--------------- add this
        -- Generator Marker
        ]
```

At the end of the file, there is a line like:

```haskell
instance InitControllerContext WebApplication
```

We need to extend this function with the following code:

```haskell
instance InitControllerContext WebApplication where
    initContext =
        initAuthentication @User
```

This will fetch the user from the database when a `userId` is given in the session. The fetched user record is saved to the special `?controllerContext` variable and is used by all the helper functions we have imported before.

#### Adding a Session Controller

In the last step, we need to add a new controller that will deal with the login and logout. We call this the `SessionsController`.

First, we have to update `Web/Types.hs`. The auth module directs users to the login page automatically if required by a view, to set this up we add the following to `Web/Types.hs`:

```haskell
import IHP.LoginSupport.Types

instance HasNewSessionUrl User where
    newSessionUrl _ = "/NewSession"
```

You also need to add the type definitions for the `SessionsController`:

```haskell
data SessionsController
    = NewSessionAction
    | CreateSessionAction
    | DeleteSessionAction
    deriving (Eq, Show, Data)
```

After that we need to set up routing for our new controller in `Web/Routes.hs`:

```haskell
instance AutoRoute SessionsController
```

We also need to create a `Web/Controller/Sessions.hs` calling the IHP authentication module:

```haskell
module Web.Controller.Sessions where

import Web.Controller.Prelude
import Web.View.Sessions.New
import qualified IHP.AuthSupport.Controller.Sessions as Sessions

instance Controller SessionsController where
    action NewSessionAction = Sessions.newSessionAction @User
    action CreateSessionAction = Sessions.createSessionAction @User
    action DeleteSessionAction = Sessions.deleteSessionAction @User

instance Sessions.SessionsControllerConfig User where
```

Additionally, we need to implement a login view at `Web/View/Sessions/New.hs` like this:

```haskell
module Web.View.Sessions.New where
import Web.View.Prelude
import IHP.AuthSupport.View.Sessions.New

instance View (NewView User) where
    html NewView { .. } = [hsx|
        <div class="h-100" id="sessions-new">
            <div class="d-flex align-items-center">
                <div class="w-100">
                    <div style="max-width: 400px" class="mx-auto mb-5">
                        {renderFlashMessages}
                        <h5>Please login:</h5>
                        {renderForm user}
                    </div>
                </div>
            </div>
        </div>
    |]

renderForm :: User -> Html
renderForm user = [hsx|
    <form method="POST" action={CreateSessionAction}>
        <div class="form-group">
            <input name="email" value={get #email user} type="email" class="form-control" placeholder="E-Mail"/>
        </div>
        <div class="form-group">
            <input name="password" type="password" class="form-control" placeholder="Password"/>
        </div>
        <button type="submit" class="btn btn-primary btn-block">Login</button>
    </form>
|]
```

## Trying out the login

After you have completed the above steps, you can open the login at `/NewSession`. You can generate a link to your login page like this:

```html
<a href="{NewSessionAction}">Login</a>
```

## Accessing the current user

Inside your actions you can then use `currentUser` to get access to the current logged in user:

```haskell
action MyAction = do
    let text = "Hello " <> (get #email currentUser)
    renderPlain text
```

In case the user is logged out, an exception will be thrown when accessing `currentUser` and the browser will automatically be redirected to the `NewSessionAction`.

You can use `currentUserOrNothing` to manually deal with the not-logged-in case:

```haskell
action MyAction = do
    case currentUserOrNothing of
        Just currentUser -> do
            let text = "Hello " <> (get #email currentUser)
            renderPlain text
        Nothing -> renderPlain "Please login first"
```

Additionally you can use `currentUserId` as a shortcut for `currentUser |> get #id`.

You can also access the user using `currentUser` inside your views:

```html
[hsx|
<h1>Hello {get #email currentUser}</h1>
|]
```

## Logout

You can simply render a link inside your layout or view to send the user to the logout:

```html
<a class="js-delete js-delete-no-confirm" href={DeleteSessionAction}>Logout</a>
```

## Creating a user

To create a user with a hashed password, you just need to call the hashing function before saving it into the database:

```haskell
    action CreateUserAction = do
        let user = newRecord @User
        user
            |> fill @["email", "passwordHash"]
            |> validateField #email isEmail
            |> validateField #passwordHash nonEmpty
            |> ifValid \case
                Left user -> render NewView { .. }
                Right user -> do
                    hashed <- hashPassword (get #passwordHash user)
                    user <- user
                        |> set #passwordHash hashed
                        |> createRecord
                    setSuccessMessage "You have registered successfully"
```

## Hashing a Password

To manually insert a user into your database you need to hash the password first. You can do this by calling the `hash-password` tool from your command line:

```bash
$ hash-password
Enter your password and press enter:
hunter2
sha256|17|Y32Ga1uke5CisJvVp6p2sg==|TSDuEs1+Xdaels6TYCkyCgIBHxWA/US7bvBlK0vHzvc=
```

Use [`hashPassword`](https://ihp.digitallyinduced.com/api-docs/IHP-AuthSupport-Authentication.html#v:hashPassword) to hash a password from inside your application.

[Next: Authorization](https://ihp.digitallyinduced.com/Guide/authorization.html)
