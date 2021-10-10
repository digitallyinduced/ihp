# Testing

This section provides some guidelines for testing your IHP applications.

```toc

```

## Setup

Start by creating a new module for your tests (perhaps for an individual controller, something like `test/Web/Controller/SomeControllerSpec.hs`), then import the IHP testing mock framework and [Hspec](http://hspec.github.io/) (a Haskell testing library). You will also need to import your project's `Main` module (this is usually where a project's [`InitControllerContext`](https://ihp.digitallyinduced.com/api-docs/IHP-ControllerSupport.html#t:InitControllerContext) instance is defined). Your imports will likely look something like this:

```haskell
module ExampleSpec where

import           Test.Hspec
import           IHP.Test.Mocking
import           IHP.FrameworkConfig (ConfigBuilder(..))
import           IHP.Prelude
import           IHP.QueryBuilder (query)

import           Web.Types
import           Web.Routes
import           Generated.Types
import           Main ()
```

## Writing a test module
The `IHP.Test.Mocking` module has functions to mock [`ModelContext`](https://ihp.digitallyinduced.com/api-docs/IHP-ModelSupport.html#t:ModelContext), [`RequestContext`](https://ihp.digitallyinduced.com/api-docs/IHP-Controller-RequestContext.html#t:RequestContext) and [`ApplicationContext`](https://ihp.digitallyinduced.com/api-docs/IHP-ApplicationContext.html#t:ApplicationContext). These contexts are created with the [`mockContext`](https://ihp.digitallyinduced.com/api-docs/IHP-Test-Mocking.html#v:mockContext) function, which requires a [`ConfigBuilder`](https://ihp.digitallyinduced.com/api-docs/IHP-FrameworkConfig.html#t:ConfigBuilder) and an IHP application as inputs. This function should be called before the tests start:

```haskell
-- a function like this probably already exists in your Config module:
makeConfig :: IO ConfigBuilder
makeConfig = ...

spec :: Spec
spec = beforeAll (makeConfig >>= mockContext WebApplication) do
  ...
```

In order to execute database queries and run controller actions, the implicit context parameters must be bound in the testing environment using [`withContext`](https://ihp.digitallyinduced.com/api-docs/IHP-Test-Mocking.html#v:withContext). Here is an example test to check there are no users in the database:

```haskell
  describe "User controller" $ do
    it "has no existing users" $ withContext do
      users <- query @User |> fetch
      users `shouldBe` []
```

The response of a controller action can be tested (but currently only the raw HTML content is returned):
```haskell
    it "responds with content" $ withContext do
      content <- mockActionResponse NewUserAction
      content `shouldBe` "<!DOCTYPE HTML>...</html>"
```

It is also possible to check the HTTP status of a controller response and response headers:
```haskell
    it "returns a redirect header" $ withContext do
      hs <- headers (mockAction NewUserAction)
      lookup "Location" hs `shouldNotBe` Nothing

    it "creates a new user" $ withParams [("a-test-param","some-value")] do
      mockActionStatus CreateUserAction `shouldReturn` status200
```

## Running a test
To execute a single test spec, load the test module into GHCI and run `hspec spec` (make sure your database server is running).

## Next steps
For more details on how to structure test suites see the [Hspec manual](http://hspec.github.io/) (a Haskell testing library). You also might want to check out the cool [Hedgehog](https://hedgehog.qa/) library for automated property tests.

## Complete example
The following is a complete example of the above code:

```haskell
module Web.Controller.ExampleSpec where

import           Network.HTTP.Types.Status (status200)

import           Test.Hspec
import           IHP.Test.Mocking
import           IHP.FrameworkConfig (ConfigBuilder(..))
import           IHP.Prelude
import           IHP.QueryBuilder (query)

import           Web.Types
import           Web.Routes
import           Generated.Types
import           Main ()

-- a function like this probably already exists in your Config module:
-- makeConfig :: IO ConfigBuilder
-- makeConfig = ...

spec :: Spec
spec = beforeAll (makeConfig >>= mockContext WebApplication) do
  describe "User controller" $ do
    it "has no existing users" $ withContext do
      users <- query @User |> fetch
      users `shouldBe` []

    it "responds with some content" $ withContext do
      content <- mockActionResponse NewUserAction
      content `shouldBe` "<!DOCTYPE HTML>...</html>"

    it "creates a new user" $ withParams [("a-test-param","some-value")] do
      mockActionStatus CreateUserAction `shouldReturn` status200

    it "returns a redirect header" $ withContext do
      hs <- headers (mockAction NewUserAction)
      lookup "Location" hs `shouldNotBe` Nothing
```
