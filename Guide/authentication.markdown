# Authentication

```toc
```

## Note

The current (pre-)release of IHP has some bugs that prevent the following documentation from working. In order to follow along you need to use a more recent version of IHP. Open `default.nix` and change the IHP dependency to point to a working git commit, e.g. `cd715f951b7325ba24dd690669d4fea2c91b1aad`:

```
    ihp = builtins.fetchGit {
        url = "https://github.com/digitallyinduced/ihp.git";
        rev = "cd715f951b7325ba24dd690669d4fea2c91b1aad";
    };
```

After that, run `make -B .envrc` to recompile.


## Introduction

IHP provides a basic authentication toolkit out of the box.

The usual convention in IHP is to call your user record `User`. When there is an admin user, we usually call the record `Admin`. In general the authentication can work with any kind of record. The only requirement is that it has an id field.

To use the authentication module, your `users` table needs to have atleast an `id`, `email`, `password_hash`, `locked_at` and `failed_login_attempts` field:
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

## Setup

Currently the authentication toolkit has to be enabled manually. We plan to do this setup using a code generator in the future. 


#### Controller Helpers

First we need to enable the controller helpers. Open `Application/Helper/Controller.hs`. It will look like this:

```haskell
module Application.Helper.Controller (
    -- To use the built in login:
    -- module IHP.LoginSupport.Helper.Controller
) where

-- Here you can add functions which are available in all your controllers

-- To use the built in login:
-- import IHP.LoginSupport.Helper.Controller
```

Enable the import and re-export for `IHP.LoginSupport.Helper.Controller` and a type instance `type instance CurrentUserRecord = User`:

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

Additionally we also need to enable the view helpers. Open `Application/Helper/View.hs`. It will look like this:

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

At the end of the file there is a line like:

```haskell
instance InitControllerContext WebApplication
```

We need to extend this function with the following code:

```haskell
instance InitControllerContext WebApplication where
    initContext =
        initAuthentication @User
```

This will fetch the user from the database when a `userId` is given in the session. The fetched user record is saved to the special `?controllerContext` variable and is used by all the helpers functions we have imported before.

#### Adding a Session Controller

In the last step we need to add a new controller which will deal with the login and logout. We call this the `SessionsController`.

First we have to update `Web/Types.hs`. The auth module directs users to the login page automatically if required by a view, to set this up we add the following to `Web/Types.hs`:

```haskell
import IHP.LoginSupport.Types

instance HasNewSessionUrl User where
    newSessionUrl _ = "/NewSession"
```

You also need to add the type definitions for the SessionsController:

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

Additionally we need to implement a login view at `Web/View/Sessions/New.hs` like this:

```haskell
module Web.View.Sessions.New where
import Web.View.Prelude
import IHP.AuthSupport.View.Sessions.New

instance View (NewView User) ViewContext where
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
<a href={NewSessionAction}>Login</a>
```

## Accessing the current user

In order to access the current user from your actions and templates you need to add it to the view context.

Update `Web/Types.hs` and add a `user` field to the ViewContext data type:

```haskell
data ViewContext = ViewContext
    { requestContext :: ControllerSupport.RequestContext
    , flashMessages :: [IHP.Controller.Session.FlashMessage]
    , controllerContext :: ControllerSupport.ControllerContext
    , layout :: Layout
    , user :: Maybe User -- <--------------- add this
    }
```

and then uncomment it in `Web/View/Context.hs`:

```haskell
let viewContext = ViewContext {
    requestContext = ?requestContext,
	user = currentUserOrNothing, -- <--------------- uncomment this line
	flashMessages,
	controllerContext = ?controllerContext,
	layout = let ?viewContext = viewContext in defaultLayout
}
```

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

[Next: Authorization](https://ihp.digitallyinduced.com/Guide/authorization.html)
