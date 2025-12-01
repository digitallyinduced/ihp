# Testing

This section provides some guidelines for testing your IHP applications. It is highly recommended to write a test for your Controller and Views to assert the logic, and reach better code quality.

```toc

```

## Setup

The following setup and tests can be viewed in the [Blog example](https://github.com/digitallyinduced/ihp-blog-example-app).

1. Add `hspec` and `ihp-hspec` in `flake.nix`
```nix
        haskellPackages = p: with p; [
            cabal-install
            # ...
            p.ihp
            
            hspec
            ihp-hspec
        ];
```
2. Rebuild environment with `devenv up`
3. Create a new `Test/Main.hs` module. Here you will import all your test specs.

```haskell
# Test/Main.hs

module Main where

import Test.Hspec
import IHP.Prelude

import Test.Controller.PostsSpec

main :: IO ()
main = hspec do
    Test.Controller.PostsSpec.tests
```
4. Add a new spec file for your controller.
```haskell
# Test/Controller/PostsSpec.hs
module Test.Controller.PostsSpec where

import Network.HTTP.Types.Status

import IHP.Prelude
import IHP.QueryBuilder (query)
import IHP.Test.Mocking
import IHP.Fetch

import IHP.FrameworkConfig
import IHP.HaskellSupport
import Test.Hspec
import Config

import Generated.Types
import Web.Routes
import Web.Types
import Web.Controller.Posts ()
import Web.FrontController ()
import Network.Wai
import IHP.ControllerPrelude
import IHP.Hspec (withIHPApp)

tests :: Spec
tests = aroundAll (withIHPApp WebApplication config) do
        describe "PostsController" $ do
            it "has no existing posts" $ withContext do
                count <- query @Post
                    |> fetchCount
                count `shouldBe` 0

            it "calling NewPostAction will render a new form" $ withContext do
                mockActionStatus NewPostAction `shouldReturn` status200

            it "creates a new post" $ withContext do
                response <- callActionWithParams CreatePostAction [("title", "Post title"), ("body", "Body of post")]

                let (Just location) = (lookup "Location" (responseHeaders response))
                location `shouldBe` "http://localhost:8000/Posts"

                -- Only one post should exist.
                count <- query @Post |> fetchCount
                count `shouldBe` 1

                -- Fetch the new post.
                post <- query @Post |> fetchOne

                post.title `shouldBe` "Post title"
                post.body `shouldBe` "Body of post"

            it "can show posts" $ withContext do
                post <- newRecord @Post
                    |> set #title "Lorem Ipsum"
                    |> set #body "**Mark down**"
                    |> createRecord

                response <- callAction ShowPostAction { postId = post.id }

                response `responseStatusShouldBe` status200
                response `responseBodyShouldContain` "Lorem Ipsum"

                -- For debugging purposes you could do the following, to
                -- see the HTML printed out on the terminal.
                body <- responseBody response
                putStrLn (cs body)
```
5. Execute the tests:
```
nix-shell
ghci
:l Test/Main
main
```

Please note that when entering `ghci` it might give a warning:

```
ghci
GHCi, version 8.10.3: https://www.haskell.org/ghc/  :? for help
*** WARNING: . is writable by someone else, IGNORING!
Suggested fix: execute 'chmod go-w .'
```

In this case, follow the suggested fix, exist ghci (`:q`) and execute `chmod go-w .`. Then you can resume the process. When ghci loads correctly it should show

```
GHCi, version 8.10.3: https://www.haskell.org/ghc/  :? for help
package flags have changed, resetting and loading new packages...
Loaded GHCi configuration from /home/amitaibu/Sites/Haskell/ihp/blog/.ghci
```

Another way of executing the tests, that we'll use on [CI](https://github.com/digitallyinduced/ihp-boilerplate/blob/master/.github/workflows/test.yml), is to use the `runghc` command, while running `devenv up` on another tab:

```
runghc $(make print-ghc-extensions) -i. -ibuild -iConfig Test/Main.hs
```

To run a particular set of tests, use `--match`.

```
runghc $(make print-ghc-extensions) -i. -ibuild -iConfig Test/Main.hs --match "Posts"
```

This command will execute all tests which are described under describe "Posts controller functionality".

## Setting the Current User During Testing

Use `withUser` to call an action with a specific user during testing:

```haskell
tests :: Spec
tests = aroundAll (withIHPApp WebApplication config) do
        describe "PostsController" $ do
            it "creates a new post" $ withContext do
                -- Create a user for our test case
                user <- newRecord @User
                    |> set #email "marc@digitallyinduced.com"
                    |> createRecord

                -- Log into the user and then call CreatePostAction
                response <- withUser user do
                    callActionWithParams CreatePostAction [("title", "Post title"), ("body", "Body of post")]

                let (Just location) = (lookup "Location" (responseHeaders response))
                location `shouldBe` "http://localhost:8000/Posts"

                -- Only one post should exist
                count <- query @Post |> fetchCount
                count `shouldBe` 1

                -- Fetch the new post
                post <- query @Post |> fetchOne

                post.title `shouldBe` "Post title"
                post.body `shouldBe` "Body of post"
```

## Test outgoing emails with Mailhog

Mailhog is a tool that allows you to test outgoing emails. It runs a fake SMTP server and a web interface to view the emails. We can use its API to to test your email sending logic, and assert the contents of the emails.

Install Mailhog by adding to `flake.nix`:

```nix
# flake.nix
# ...
perSystem = { pkgs, ... }: {
    ihp = {
        enable = true;
        projectPath = ./.;
        packages = with pkgs; [
            # Native dependencies, e.g. imagemagick
            # Used for local development
            mailhog
        ];
        haskellPackages = p: with p; [
            # Haskell dependencies go here
            p.ihp
            cabal-install
            base
            wai
            text
            hlint
            hspec
        ];
    };

    # Start mailhog on `devenv up`.
    devenv.shells.default = {
        services.mailhog.enable = true;
    };
};
```

Notice we've also enabled the `mailhog` service in `devenv.shells.default`. This will start Mailhog when you run `devenv up`, which is useful for local development.

Follow the instructions on how to add a Mail action, and how to configure the SMTP on the [Mail page](https://ihp.digitallyinduced.com/Guide/mail.html).

Let's see how we can test a Mail that is sent every time a `Post` is being shown.

```haskell
-- Web/Mail/Posts/PostView.hs

module Web.Mail.Posts.PostView where
import Web.View.Prelude
import IHP.MailPrelude

data PostViewMail = PostViewMail { post :: Post }

instance BuildMail PostViewMail where
    subject = "Showing Post " <> post.title
        where post = ?mail.post
    to PostViewMail { .. } = Address { addressName = Just "Firstname Lastname", addressEmail = "fname.lname@example.com" }
    from = "hi@example.com"
    html PostViewMail { .. } = [hsx|
        A post was just viewed
    |]
```

And let's trigger this email on the `Post` show:

```haskell
-- Web/Controller/Posts.hs

action ShowPostAction { .. } = do
    post <- fetch postId

    -- Send mail.
    sendMail $ PostViewMail post

    render ShowView { .. }
```

Now we can test is the email is being sent by using the Mailhog API:

```haskell
-- Test/PostSpec.hs

module Test.PostSpec where

import IHP.Prelude
import IHP.FrameworkConfig
import IHP.Test.Mocking
import IHP.HaskellSupport
import IHP.ModelSupport
import Test.Hspec
import Config

import Generated.Types
import Web.Routes
import Web.Types
import Web.FrontController
import Network.Wai
import IHP.ControllerPrelude
import IHP.ViewPrelude hiding (query)
import Data.Text as Text
import Network.HTTP.Types.Status
import Network.HTTP.Client
import qualified Network.Wreq as Wreq
import Control.Lens ((^.))
import IHP.Hspec (withIHPApp)


tests :: Spec
tests = aroundAll (withIHPApp WebApplication config) do
    describe "Post" do
        it "should send an email on each page view" $ withContext do
            -- Optional: delete any previous emails in Mailhog.
            Wreq.delete "http://0.0.0.0:8025/api/v1/messages"

            -- Get random title.
            title <- generateAuthenticationToken

            -- Create a Post.
            post <- newRecord @Post
                |> set #title title
                |> createRecord

            response <-
                callAction $ ShowPostAction post.id

            -- Assert email was sent, and caught by Mailhog.
            documentBody <- do
                response <- Wreq.get "http://0.0.0.0:8025/api/v1/messages"
                pure (response ^. Wreq.responseBody)

            cs documentBody `shouldContain` ("Post " <> cs post.title)
```

## Advanced
For more details on how to structure test suites see the [Hspec manual](http://hspec.github.io/) (a Haskell testing library). You also might want to check out the cool [Hedgehog](https://hedgehog.qa/) library for automated property tests.

## GitHub Actions

A GitHub Action workflow can be used to run the tests on CI and do deployments. Consult the [IHP Boilerplate example](https://github.com/digitallyinduced/ihp-boilerplate/blob/master/.github/workflows/test.yml) for more details.
