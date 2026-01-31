{-|
Module: Test.LoginSupport.AuthVaultSpec
Copyright: (c) digitally induced GmbH, 2025
-}
module Test.LoginSupport.AuthVaultSpec where

import IHP.Prelude
import Test.Hspec

import qualified Data.Vault.Lazy as Vault
import qualified Network.Wai as Wai
import Network.Wai.Test (defaultRequest, runSession, request)
import Network.HTTP.Types.Status (status200)
import IHP.LoginSupport.Types (CurrentUserRecord, CurrentAdminRecord, currentUserVaultKey, currentAdminVaultKey)
import IHP.LoginSupport.Middleware (authMiddlewareWith)
import IHP.Controller.Context
import IHP.RequestVault (requestBodyVaultKey)
import Wai.Request.Params.Middleware (RequestBody (..))
import qualified IHP.LoginSupport.Helper.Controller as Controller
import qualified IHP.LoginSupport.Helper.View as View

tests :: Spec
tests = do
    describe "Auth Middleware" do
        describe "user authentication" do
            it "populates Controller.currentUserOrNothing when user is authenticated" do
                let user = TestUser { id = "00000000-0000-0000-0000-000000000001" }
                let app = userMiddleware (Just user) $ \req respond -> do
                        ctx <- createControllerContext req
                        let ?context = ctx
                        Controller.currentUserOrNothing `shouldBe` Just user
                        respond $ Wai.responseLBS status200 [] ""
                flip runSession app $ request defaultRequest >> pure ()

            it "Controller.currentUserOrNothing returns Nothing when not authenticated" do
                let app = userMiddleware Nothing $ \req respond -> do
                        ctx <- createControllerContext req
                        let ?context = ctx
                        (Controller.currentUserOrNothing :: Maybe CurrentUserRecord) `shouldBe` Nothing
                        respond $ Wai.responseLBS status200 [] ""
                flip runSession app $ request defaultRequest >> pure ()

            it "populates View.currentUserOrNothing when user is authenticated" do
                let user = TestUser { id = "00000000-0000-0000-0000-000000000001" }
                let app = userMiddleware (Just user) $ \req respond -> do
                        ctx <- createControllerContext req
                        let ?context = ctx
                        View.currentUserOrNothing `shouldBe` Just user
                        respond $ Wai.responseLBS status200 [] ""
                flip runSession app $ request defaultRequest >> pure ()

        describe "admin authentication" do
            it "populates Controller.currentAdminOrNothing when admin is authenticated" do
                let admin = TestAdmin { id = "00000000-0000-0000-0000-000000000002" }
                let app = adminMiddleware (Just admin) $ \req respond -> do
                        ctx <- createControllerContext req
                        let ?context = ctx
                        Controller.currentAdminOrNothing `shouldBe` Just admin
                        respond $ Wai.responseLBS status200 [] ""
                flip runSession app $ request defaultRequest >> pure ()

            it "Controller.currentAdminOrNothing returns Nothing when not authenticated" do
                let app = adminMiddleware Nothing $ \req respond -> do
                        ctx <- createControllerContext req
                        let ?context = ctx
                        (Controller.currentAdminOrNothing :: Maybe CurrentAdminRecord) `shouldBe` Nothing
                        respond $ Wai.responseLBS status200 [] ""
                flip runSession app $ request defaultRequest >> pure ()

        describe "middleware composition" do
            it "supports both user and admin simultaneously" do
                let user = TestUser { id = "00000000-0000-0000-0000-000000000001" }
                let admin = TestAdmin { id = "00000000-0000-0000-0000-000000000002" }
                let composed = userMiddleware (Just user) . adminMiddleware (Just admin)
                let app = composed $ \req respond -> do
                        ctx <- createControllerContext req
                        let ?context = ctx
                        Controller.currentUserOrNothing `shouldBe` Just user
                        Controller.currentAdminOrNothing `shouldBe` Just admin
                        respond $ Wai.responseLBS status200 [] ""
                flip runSession app $ request defaultRequest >> pure ()

            it "user middleware does not affect admin lookup" do
                let user = TestUser { id = "00000000-0000-0000-0000-000000000001" }
                let app = userMiddleware (Just user) $ \req respond -> do
                        ctx <- createControllerContext req
                        let ?context = ctx
                        (Controller.currentAdminOrNothing :: Maybe CurrentAdminRecord) `shouldBe` Nothing
                        respond $ Wai.responseLBS status200 [] ""
                flip runSession app $ request defaultRequest >> pure ()

-- | Mock user type for tests
data TestUser = TestUser { id :: UUID }
    deriving (Eq, Show)

-- | Mock admin type for tests
data TestAdmin = TestAdmin { id :: UUID }
    deriving (Eq, Show)

type instance CurrentUserRecord = TestUser
type instance CurrentAdminRecord = TestAdmin

-- | Middleware that stores a known user, using the same code path as the real middleware.
userMiddleware :: Maybe TestUser -> Wai.Middleware
userMiddleware user = authMiddlewareWith currentUserVaultKey (\_ -> pure user)

-- | Middleware that stores a known admin, using the same code path as the real middleware.
adminMiddleware :: Maybe TestAdmin -> Wai.Middleware
adminMiddleware admin = authMiddlewareWith currentAdminVaultKey (\_ -> pure admin)

-- | Create a controller context from a WAI request.
createControllerContext :: Wai.Request -> IO ControllerContext
createControllerContext req = do
    let requestBody = FormBody { params = [], files = [] }
    let requestWithBody = req { Wai.vault = Vault.insert requestBodyVaultKey requestBody (Wai.vault req) }
    let ?request = requestWithBody
    newControllerContext
