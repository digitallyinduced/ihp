# OAuth

```toc
```

## Introduction

Next to normal email login, IHP also provides support for OAuth-login with third-party identity providers. Login via OAuth requires IHP Pro.

At the moment IHP supports these identity providers:

- Google
- GitHub

## Login with Google

### Setup

To use the Login with Google functionality you first need to enable the `ihp-oauth-google` package.

Open your project's `default.nix` and a `ihp-oauth-google` dependency to `haskellDeps`:

```nix
let
    ihp = ..
    haskellEnv = import "${ihp}/NixSupport/default.nix" {
        ihp = ihp;
        haskellDeps = p: with p; [
            # ...

            ihp-oauth-google # <----- ADD THIS LINE
        ];
        otherDeps = p: with p; [
        ];
        projectPath = ./.;
    };
in
    haskellEnv
```

After that stop your local development server and run the following command to install the package:

```
make -B .envrc
```

Now you can run `./start` again and the `ihp-oauth-google` package is enabled.

### Schema Changes

For Google OAuth Login to work, we need to add a `google_user_id TEXT` column to our `users` table. The column needs to be nullable and have `NULL` as the default value.

Open `Application/Schema.sql` and add `google_user_id TEXT` to the `CREATE TABLE users` statement:
```sql
CREATE TABLE users (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    -- ... ,
    google_user_id TEXT
);
```

### Creating the Controller

Next we need to add the `GoogleOAuth` controller. For that create a new file `Web/Controller/GoogleOAuth.hs` with the following content:

```haskell
module Web.Controller.GoogleOAuth where

import Web.Controller.Prelude
import Web.Controller.Sessions ()
import IHP.OAuth.Google.Controller
import qualified IHP.OAuth.Google.Types as Google

instance Controller Google.GoogleOAuthController where
    action Google.NewSessionWithGoogleAction = newSessionWithGoogleAction @User
    action Google.GoogleConnectCallbackAction = googleConnectCallbackAction @User


instance GoogleOAuthControllerConfig User where
    beforeCreateUser user googleClaims =  user |> set #isConfirmed True -- This will auto-confirm your user's email
```

#### Routing

We also need to enable routing for this controller. Open `Web/Routes.hs` and

1. Import the type definition by adding this import to the top of the file:
    ```haskell
    import IHP.OAuth.Google.Types
    ```
2. Enable AutoRoute by adding this to end of the file:
    ```haskell
    instance AutoRoute GoogleOAuthController
    ```

#### FrontController

Next we need to enable this controller inside the FrontController, so that requests can be routed there.

Open `Web/FrontController.hs` and

1. Add the following imports to the top of the file:
    ```haskell
    import IHP.OAuth.Google.Types
    import Web.Controller.GoogleOAuth
    ```
2. Add `GoogleOAuthController` to the list of controllers:
    ```haskell
    instance FrontController WebApplication where
        controllers = 
            [ startPage StartpageAction
            -- ...
            , parseRoute @GoogleOAuthController -- <----- ADD THIS
            ]
    ```

#### View Prelude

As the types for the controller are defined in `IHP.OAuth.Google.Types` instead of the usual `Web.Types` we need to make changes to our `Web/View/Prelude.hs`:

```haskell
module Web.View.Prelude
( module IHP.ViewPrelude
-- ...
, module IHP.OAuth.Google.Types -- <---- ADD THIS
) where

import IHP.ViewPrelude
-- ...
import IHP.OAuth.Google.Types  -- <---- ADD THIS
```

This ensures that we can write `pathTo NewSessionWithGoogleAction` and similiar calls without always manually needing to add import statements.

### Config

Before the controller can be used, we need to configure the google client id. A client id looks like this: `1234567890-abc123def456.apps.googleusercontent.com`.

You can create a google client id by logging into the Google API console and adding a new project. Additionally you need to enable and configure OAuth inside the API console. [Follow this Guide by Google if you haven't done this before.](https://developers.google.com/identity/gsi/web/guides/get-google-api-clientid)

**Once you got your client id, you can continue here:**

1. Open `Config/Config.hs`
2. Add this import:
    ```haskell
    import IHP.OAuth.Google.Config
    ```
3. Call `initGoogleOAuth`:
    ```haskell
    config :: ConfigBuilder
    config = do
        option Development
        option (AppHostname "localhost")

        initGoogleOAuth -- <--- ADD THIS
    ```

The call to `initGoogleOAuth` will read the `OAUTH_GOOGLE_CLIENT_ID` environment variable and pass it to the `GoogleOAuthController`. We also need to define this environment variable locally. For that open the `start` script in your project and add a `OAUTH_GOOGLE_CLIENT` statement like this:

```bash
# ...

export OAUTH_GOOGLE_CLIENT_ID="MY_GOOGLE_CLIENT_ID" # <-- ADD THIS LINE BEFORE THE RunDevServer CALL

RunDevServer
```

You need to replace the `MY_GOOGLE_CLIENT_ID` with your google client id. After making the changes to `start`, you need to restart your local dev server.

### Trying it out

Now that Google OAuth is configured, we can already test it out.

Open `http://localhost:8000/NewSessionWithGoogle` and click on the `Login with Google` button. It should open a google login dialog and then continue logging into the app.

If google shows you an error message, check the javascript error console for details. A common error is that the `localhost:8000` host is not on the host whiteliste of the Google Client ID.

### Optional: Adding the Login with Google Button

Typically you don't want to redirect your users to `http://localhost:8000/NewSessionWithGoogle`, instead you want to place a `Login with Google` button on your existing login page.

Open `Web/Sessions/New.hs` and apply these changes:

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
                        <h5>Please login</h5>
                        {renderForm user}


                        {loginWithGoogle} <!-- ADD THIS CALL HERE -->


                    </div>
                </div>
            </div>
        </div>
    |]

-- ...

loginWithGoogle :: Html
loginWithGoogle = [hsx|
    <a id="continue-with-google" href="#" class="btn btn-primary" data-client-id="YOUR GOOGLE CLIENT ID">
        Continue with Google
    </a>
    <form method="POST" action={GoogleConnectCallbackAction} id="new-session-with-google-form">
        <input type="hidden" name="jwt" value=""/>
    </form>
    <script src="/google-login.js"></script>
    <script src="https://apis.google.com/js/platform.js?onload=initGoogleLogin"></script>
|]
```

The login process uses the Google Login JS SDK and requires a little bit of javascript to work. Create a file `static/google-login.js` with this content:

```javascript
function initGoogleLogin() {
    gapi.load('auth2', function() {
        var element = document.getElementById('continue-with-google');
        var clientId = element.dataset.clientId;
        auth2 = gapi.auth2.init({ client_id: clientId, scope: 'profile' });
        
        auth2.attachClickHandler(element, {},
            function(googleUser) {
                var form = document.getElementById('new-session-with-google-form');
                form.querySelector('input[name="jwt"]').value = googleUser.getAuthResponse().id_token;
                form.submit();
            }, function(error) {
                alert(JSON.stringify(error, undefined, 2));
            });
    });
}
```

Now you should be able to log in with Google from your normal login page.

## Login with GitHub

### Setup

To use the Login with GitHub functionality you first need to enable the `ihp-oauth-github` package.

Open your project's `default.nix` and a `ihp-oauth-github` dependency to `haskellDeps`:

```nix
let
    ihp = ..
    haskellEnv = import "${ihp}/NixSupport/default.nix" {
        ihp = ihp;
        haskellDeps = p: with p; [
            # ...

            ihp-oauth-github # <----- ADD THIS LINE
        ];
        otherDeps = p: with p; [
        ];
        projectPath = ./.;
    };
in
    haskellEnv
```

After that stop your local development server and run the following command to install the package:

```
make -B .envrc
```

Now you can run `./start` again and the `ihp-oauth-github` package is enabled.


### Schema Changes

For GitHub OAuth Login to work, we need to add a `github_user_id INT` column to our `users` table. The column needs to be nullable and have `NULL` as the default value.

Open `Application/Schema.sql` and add `github_iser_id INT` to the `CREATE TABLE users` statement:
```sql
CREATE TABLE users (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    -- ... ,
    github_user_id INT DEFAULT NULL UNIQUE,
);
```


### Creating the Controller

Next we need to add the `GithubOAuth` controller. For that create a new file `Web/Controller/GithubOAuth.hs` with the following content:

```haskell
module Web.Controller.GithubOAuth where

import Web.Controller.Prelude
import Web.Controller.Sessions ()
import IHP.OAuth.Github.Controller
import qualified IHP.OAuth.Github.Types as Github

instance Controller Github.GithubOAuthController where
    action Github.NewSessionWithGithubAction = newSessionWithGithubAction @User
    action Github.GithubConnectCallbackAction = githubConnectCallbackAction @User


instance GithubOAuthControllerConfig User where
    beforeCreateUser user githubUser = user |> set #isConfirmed True -- This will auto-confirm your user's email. Remove this line if you don't use email confirmation


    afterCreateUser user = do
        -- Called after a new user is created
        -- E.g. you could send a welcome email here:
        --
        -- > sendMail WelcomeMail { .. }
        --
        pure ()

    beforeLogin user githubUser = do
        -- Called before the user is logged in
        -- Here you can e.g. set the app's profile picture to the one provided by github:
        --
        -- > user
        -- >     |> setJust #profilePicture (get #avatarUrl githubUser)
        -- >     |> setJust #githubName (get #login githubUser)
        -- >     |> pure
        --
        pure user
```

#### Routing

We also need to enable routing for this controller. Open `Web/Routes.hs` and

1. Import the type definition by adding this import to the top of the file:
    ```haskell
    import IHP.OAuth.Github.Types
    ```
2. Enable AutoRoute by adding this to end of the file:
    ```haskell
    instance AutoRoute GithubOAuthController
    ```

#### FrontController

Next we need to enable this controller inside the FrontController, so that requests can be routed there.

Open `Web/FrontController.hs` and

1. Add the following imports to the top of the file:
    ```haskell
    import IHP.OAuth.Github.Types
    import Web.Controller.GithubOAuth
    ```
2. Add `GithubOAuthController` to the list of controllers:
    ```haskell
    instance FrontController WebApplication where
        controllers = 
            [ startPage StartpageAction
            -- ...
            , parseRoute @GithubOAuthController -- <----- ADD THIS
            ]
    ```

#### View Prelude

As the types for the controller are defined in `IHP.OAuth.Github.Types` instead of the usual `Web.Types` we need to make changes to our `Web/View/Prelude.hs`:

```haskell
module Web.View.Prelude
( module IHP.ViewPrelude
-- ...
, module IHP.OAuth.Github.Types -- <---- ADD THIS
) where

import IHP.ViewPrelude
-- ...
import IHP.OAuth.Github.Types  -- <---- ADD THIS
```

This ensures that we can write `pathTo NewSessionWithGithubAction` and similiar calls without always manually needing to add import statements.


### Config

Before the controller can be used, we need to configure the github client id and client secret. 

[Follow this Guide by GitHub to create a GitHub app and get your client id and secret.](https://docs.github.com/en/developers/apps/building-github-apps/creating-a-github-app). During the setup you need to provide a **Callback URL**. Set this to `http://localhost:8000/GithubConnectCallback`.

**Once you got your client id and secret, you can continue here:**

1. Open `Config/Config.hs`
2. Add this import:
    ```haskell
    import IHP.OAuth.Github.Config
    ```
3. Call `initGithubOAuth`:
    ```haskell
    config :: ConfigBuilder
    config = do
        option Development
        option (AppHostname "localhost")

        initGithubOAuth -- <--- ADD THIS
    ```

The call to `initGithubOAuth` will read the `OAUTH_GITHUB_CLIENT_ID` and `OAUTH_GITHUB_CLIENT_SECRET` environment variables, and pass it to the `GithubOAuthController`. We also need to define this environment variable locally. For that open the `start` script in your project and add two statements like this:

```bash
# ...

export OAUTH_GITHUB_CLIENT_ID="MY_GITHUB_CLIENT_ID" # <-- ADD THIS LINE BEFORE THE RunDevServer CALL
export OAUTH_GITHUB_CLIENT_SECRET="MY_GITHUB_SECRET"

RunDevServer
```

You need to replace `MY_GITHUB_CLIENT_ID` and `MY_GITHUB_SECRET` with your values. After making the changes to `start`, you need to restart your local dev server.

### Trying it out

Now Github Login is ready and we can try it out.

Open `http://localhost:8000/NewSessionWithGithub` and click on the `Login with Github` button. It will redirect to github and then continue logging into the app.

You can place a `Login with Github` button by linking to the `NewSessionWithGithubAction`:

```haskell
<a href={NewSessionWithGithubAction} target="_self" class="btn btn-primary">
    Continue with GitHub
</a>
```

Use `target="_self"` for these links to avoid turbolinks interfering with the redirect to github.

Now your github integration is ready to use.
