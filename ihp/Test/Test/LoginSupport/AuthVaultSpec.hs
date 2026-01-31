{-|
Module: Test.LoginSupport.AuthVaultSpec
Copyright: (c) digitally induced GmbH, 2025
-}
module Test.LoginSupport.AuthVaultSpec where

import IHP.Prelude
import Test.Hspec
import Data.IORef

import qualified Data.Vault.Lazy as Vault
import qualified Network.Wai as Wai
import Network.Wai.Test (defaultRequest)
import Network.Wai.Internal (ResponseReceived(..))
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
                result <- runMiddleware (userMiddleware (Just user)) $ \req -> do
                    context <- createControllerContext req
                    let ?context = context
                    pure Controller.currentUserOrNothing
                result `shouldBe` Just user

            it "Controller.currentUserOrNothing returns Nothing when not authenticated" do
                result <- runMiddleware (userMiddleware Nothing) $ \req -> do
                    context <- createControllerContext req
                    let ?context = context
                    pure (Controller.currentUserOrNothing :: Maybe CurrentUserRecord)
                result `shouldBe` Nothing

            it "populates View.currentUserOrNothing when user is authenticated" do
                let user = TestUser { id = "00000000-0000-0000-0000-000000000001" }
                result <- runMiddleware (userMiddleware (Just user)) $ \req -> do
                    context <- createControllerContext req
                    let ?context = context
                    pure View.currentUserOrNothing
                result `shouldBe` Just user

        describe "admin authentication" do
            it "populates Controller.currentAdminOrNothing when admin is authenticated" do
                let admin = TestAdmin { id = "00000000-0000-0000-0000-000000000002" }
                result <- runMiddleware (adminMiddleware (Just admin)) $ \req -> do
                    context <- createControllerContext req
                    let ?context = context
                    pure Controller.currentAdminOrNothing
                result `shouldBe` Just admin

            it "Controller.currentAdminOrNothing returns Nothing when not authenticated" do
                result <- runMiddleware (adminMiddleware Nothing) $ \req -> do
                    context <- createControllerContext req
                    let ?context = context
                    pure (Controller.currentAdminOrNothing :: Maybe CurrentAdminRecord)
                result `shouldBe` Nothing

        describe "middleware composition" do
            it "supports both user and admin simultaneously" do
                let user = TestUser { id = "00000000-0000-0000-0000-000000000001" }
                let admin = TestAdmin { id = "00000000-0000-0000-0000-000000000002" }
                let composed = userMiddleware (Just user) . adminMiddleware (Just admin)
                result <- runMiddleware composed $ \req -> do
                    context <- createControllerContext req
                    let ?context = context
                    pure (Controller.currentUserOrNothing, Controller.currentAdminOrNothing)
                result `shouldBe` (Just user, Just admin)

            it "user middleware does not affect admin lookup" do
                let user = TestUser { id = "00000000-0000-0000-0000-000000000001" }
                result <- runMiddleware (userMiddleware (Just user)) $ \req -> do
                    context <- createControllerContext req
                    let ?context = context
                    pure (Controller.currentAdminOrNothing :: Maybe CurrentAdminRecord)
                result `shouldBe` Nothing

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

-- | Run a WAI middleware and extract a value from the downstream application.
runMiddleware :: Wai.Middleware -> (Wai.Request -> IO a) -> IO a
runMiddleware middleware extract = do
    resultRef <- newIORef (error "runMiddleware: app was not called")
    let app req respond = do
            result <- extract req
            writeIORef resultRef result
            respond (Wai.responseLBS status200 [] "")
    _ <- middleware app defaultRequest (\_ -> pure ResponseReceived)
    readIORef resultRef

-- | Create a controller context from a WAI request.
createControllerContext :: Wai.Request -> IO ControllerContext
createControllerContext request = do
    let requestBody = FormBody { params = [], files = [] }
    let requestWithBody = request { Wai.vault = Vault.insert requestBodyVaultKey requestBody (Wai.vault request) }
    let ?request = requestWithBody
    newControllerContext
