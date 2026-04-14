{-|
Module: Test.MockingSpec
Tests for IHP.Test.Mocking, ensuring params are correctly delivered
through the middleware stack and that redirect responses preserve
their status codes.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.MockingSpec where
import ClassyPrelude
import Test.Hspec
import IHP.Test.Mocking
import IHP.Prelude
import IHP.Environment
import IHP.FrameworkConfig
import IHP.ControllerPrelude hiding (get, request)
import IHP.HSX.Markup (Html)
import Network.Wai.Test
import Test.Util (testGet, testPostForm)

import qualified Data.UUID as UUID
import qualified Data.Vault.Lazy as Vault
import qualified Network.Wai as Wai
import Network.HTTP.Types.Status (status200)
import IHP.LoginSupport.Middleware (authMiddlewareWith, parseSessionUUID)
import System.IO.Unsafe (unsafePerformIO)

data WebApplication = WebApplication deriving (Eq, Show, Data)

data TestController
    = EchoParamAction
    | RedirectAction
    | RequireUserAction
  deriving (Eq, Show, Data)

instance Controller TestController where
    action EchoParamAction = do
        let value = param @Text "message"
        renderPlain (cs value)
    action RedirectAction = do
        redirectToPath "/target"
    action RequireUserAction = do
        -- Returns the authenticated TestUser's id (set by testAuthMiddleware
        -- when the mock session inserted by 'withUser' survives through
        -- 'sessionMiddleware'), or "anonymous" otherwise.
        let maybeUser = join (Vault.lookup testUserVaultKey ?request.vault)
        case maybeUser of
            Just (TestUser { id = Id uuid }) -> renderPlain (cs (UUID.toASCIIBytes uuid))
            Nothing -> renderPlain "anonymous"

instance AutoRoute TestController

instance FrontController WebApplication where
  controllers = [ parseRoute @TestController ]

defaultLayout :: Html -> Html
defaultLayout inner = inner

instance InitControllerContext WebApplication where
  initContext = do
    setLayout defaultLayout

instance FrontController RootApplication where
    controllers = [ mountFrontController WebApplication ]

config = do
    option Development
    option (AppPort 8000)
    option $ AuthMiddleware testAuthMiddleware

-- | Synthetic user type used by the auth-middleware regression test.
-- Defined here rather than importing a real model so the test does not
-- need a database table.
data TestUser = TestUser { id :: Id' "test_users" }
    deriving (Eq, Show)

type instance GetTableName TestUser = "test_users"
type instance PrimaryKey "test_users" = UUID
type instance GetModelName TestUser = "TestUser"

-- | Vault key where 'testAuthMiddleware' stashes the authenticated user.
-- Created once at module load and reused for every request.
{-# NOINLINE testUserVaultKey #-}
testUserVaultKey :: Vault.Key (Maybe TestUser)
testUserVaultKey = unsafePerformIO Vault.newKey

-- | A custom 'AuthMiddleware' that reads the session written by 'withUser'
-- (under @login.TestUser@, cereal-encoded UUID) and stores the parsed
-- 'TestUser' in 'testUserVaultKey'. This mirrors what
-- @authMiddleware \@User@ does in a real app but without touching the
-- database, so the test exercises the full middleware stack — including
-- the @wai-session-maybe@ 'sessionMiddleware' that would otherwise wipe
-- out the mock session before this middleware runs.
testAuthMiddleware :: Wai.Middleware
testAuthMiddleware = authMiddlewareWith testUserVaultKey fetchTestUser
  where
    fetchTestUser req = case lookupSessionVault req of
        Nothing -> pure Nothing
        Just (lookupFn, _) -> do
            raw <- lookupFn "login.TestUser"
            pure $ case raw of
                Nothing -> Nothing
                Just bs -> TestUser . Id <$> parseSessionUUID bs

tests :: Spec
tests = aroundAll (withMockContextAndApp WebApplication config) do
    describe "IHP.Test.Mocking" do
        describe "form params" do
            it "should deliver params to the controller action" $ withContextAndApp \application -> do
                runSession (do
                    response <- testPostForm "test/EchoParam" [("message", "hello world")]
                    assertStatus 200 response
                    assertBody "hello world" response
                    ) application

            it "should return status 200 for rendered responses" $ withContextAndApp \application -> do
                runSession (do
                    testPostForm "test/EchoParam" [("message", "test")] >>= assertStatus 200
                    ) application

        describe "redirectTo" do
            it "should return status 302" $ withContextAndApp \application -> do
                runSession (do
                    testGet "test/Redirect" >>= assertStatus 302
                    ) application

        describe "withUser + AuthMiddleware" do
            -- Regression test for a timing bug between 'withUser' and the
            -- WAI-based 'authMiddleware'. 'withUser' drops a mock session
            -- into the request vault, but 'sessionMiddleware' (from
            -- wai-session-maybe) unconditionally overwrites the session
            -- vault key with a fresh empty session built from the request
            -- cookie. Before the fix that wiped out the mock session before
            -- 'authMiddleware' could read it, so every protected action
            -- saw no user.
            it "propagates the mock session from withUser to authMiddleware" $ withContextAndApp \_ -> do
                let Just uuid = UUID.fromString "00000000-0000-0000-0000-000000000001"
                let user = TestUser { id = Id uuid }
                response <- withUser user do
                    callAction RequireUserAction
                bodyBytes <- responseBody response
                Wai.responseStatus response `shouldBe` status200
                bodyBytes `shouldBe` cs (UUID.toASCIIBytes uuid)

            it "returns anonymous when withUser is not used" $ withContextAndApp \_ -> do
                response <- callAction RequireUserAction
                bodyBytes <- responseBody response
                Wai.responseStatus response `shouldBe` status200
                bodyBytes `shouldBe` "anonymous"
