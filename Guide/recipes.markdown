# Recipes

This section describes best-practise solutions to the common tasks you are facing when building web applications.

```toc
```

## Static Pages

For adding a static page like e.g. a start page, terms of service, privacy, pricing etc. you usually use a normal controller which just renders the view for that page. The only special thing is, that you might want to customize the routing to have SEO-friendly urls.

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

## Adding a native dependency

Sometimes your project uses some other software tool which is not bundled with IHP by default. Because we're using nix, we can easily manage that dependency for our project.

Let's say we want to add imagemagick to transform and resize images uploaded by the users of our application.

All dependencies of our project are listed in `default.nix` at the root of the project directory. The file looks like this:

```nix
let
    ihp = builtins.fetchGit {
        url = "https://github.com/digitallyinduced/ihp.git";
        rev = "c6d40612697bb7905802f23b7753702d33b9e2c1";
    };
    haskellEnv = import "${ihp}/NixSupport/default.nix" {
        compiler = "ghc865";
        haskellDeps = p: with p; [
            cabal-install
            base
            classy-prelude
            wai
            text
            hlint
            ihp
            wreq
        ];
        otherDeps = p: with p; [
        ];
        projectPath = ./.;
    };
in
    haskellEnv
```

We now just have to add `imagemagick` to `otherDeps`:

```nix
let
    ihp = builtins.fetchGit {
        url = "https://github.com/digitallyinduced/ihp.git";
        rev = "c6d40612697bb7905802f23b7753702d33b9e2c1";
    };
    haskellEnv = import "${ihp}/NixSupport/default.nix" {
        compiler = "ghc865";
        haskellDeps = p: with p; [
            cabal-install
            base
            classy-prelude
            wai
            text
            hlint
            ihp
            wreq
        ];
        otherDeps = p: with p; [

            imagemagick # <-----------------------
            
        ];
        projectPath = ./.;
    };
in
    haskellEnv
```

If running, stop your development server. Now run `make` again. This will install imagemagick locally to your project.

When you are inside the project with your terminal, you can also call `imagemagick` to see that it's available.

You can look up the package name for the software you dependend on inside the nixpkgs repository. [Just open it on GitHub](https://github.com/NixOS/nixpkgs) and use the GitHub search to look up the package name.

## Uploading a user profile picture

You can easily upload a user profile pictures using `uploadImageWithOptions` inside your `UpdateUserAction`:

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

This accepts any kind of image file compatible with imagemagick, resize it, reduce the image quality, stripe all meta information and save it as jpg. The file is stored inside the `static/uploads` folder in the project (directory will be created if it does not exist).

In your view, just use the image url like `<img src={get #pictureUrl currentUser}/>`.

## Checking that the current user has permission to access the action

Use [accessDeniedUnless](https://ihp.digitallyinduced.com/api-docs/IHP-LoginSupport-Helper-Controller.html#v:accessDeniedUnless) like this:

```haskell
action EditPostAction { postId } = do
    post <- fetch postId
    accessDeniedUnless (get #authorId post == currentUserId)
    
    renderHtml EditView { .. }
```

## Creating a custom validator

If needed you can just write your own constraint, e.g. like this:

```haskell
nonEmpty :: Text -> ValidatorResult
nonEmpty "" = Failure "This field cannot be empty"
nonEmpty _ = Success

isAge :: Int -> ValidatorResult
isAge = isInRange (0, 100)
```

## Checking that an email is unique

Use [`validateIsUnique`](https://ihp.digitallyinduced.com/api-docs/IHP-ValidationSupport-ValidateIsUnique.html#v:validateIsUnique).
