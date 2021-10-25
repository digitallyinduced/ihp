# Testing

This section provides some guidelines for testing your IHP applications. It is highly recommended to write a test for your Controller and Views to assert the logic, and reach better code quality.

```toc

```

## Setup

The following setup and tests can be viewed in the [Blog example](https://github.com/digitallyinduced/ihp-blog-example-app).

1. Add `hspec` in `default.nix`
```nix
        haskellDeps = p: with p; [
            cabal-install
            # ...
            p.ihp
            hspec
        ];
```
2. Rebuild enviorement with `make -B .envrc`
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

tests :: Spec
tests = aroundAll (withIHPApp WebApplication config) do
        describe "PostsController" $ do
            it "has no existing posts" $ withContext do
                count <- query @Post
                    |> fetchCount
                count `shouldBe` 0

            it "calling NewPostAction will render a new form" $ withContext do
                mockActionStatus NewPostAction `shouldReturn` status200

            it "creates a new post" do
                response <- callActionWithParams CreatePostAction [("title", "Post title"), ("body", "Body of post")]

                let (Just location) = (lookup "Location" (responseHeaders response))
                location `shouldBe` "http://localhost:8000/Posts"

                -- Only one post should exist.
                count <- query @Post |> fetchCount
                count `shouldBe` 1

                -- Fetch the new post.
                post <- query @Post |> fetchOne

                get #title post `shouldBe` "Post title"
                get #body post `shouldBe` "Body of post"

            it "can show posts" $ withContext do
                post <- newRecord @Post
                    |> set #title "Lorem Ipsum"
                    |> set #body "**Mark down**"
                    |> createRecord

                response <- callAction ShowPostAction { postId = get #id post }

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

## Setting the Current User During Testing

Use `withUser` to call an action with a specific user during testing:

```haskell
tests :: Spec
tests = aroundAll (withIHPApp WebApplication config) do
        describe "PostsController" $ do
            it "creates a new post" do
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

                (get #title post) `shouldBe` "Post title"
                (get #body post) `shouldBe` "Body of post"
```


## Advanced
For more details on how to structure test suites see the [Hspec manual](http://hspec.github.io/) (a Haskell testing library). You also might want to check out the cool [Hedgehog](https://hedgehog.qa/) library for automated property tests.
