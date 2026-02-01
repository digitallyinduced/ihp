{-|
Module: Test.LoginSupport.AuthVaultSpec
Copyright: (c) digitally induced GmbH, 2025
-}
module Test.LoginSupport.AuthVaultSpec where

import IHP.Prelude
import Test.Hspec

import qualified Data.ByteString as BS
import qualified Data.UUID as UUID
import qualified Network.Wai as Wai
import Network.Wai.Test (defaultRequest, runSession, request)
import Network.HTTP.Types.Status (status200)
import IHP.LoginSupport.Types (CurrentUserRecord, CurrentAdminRecord, currentUserVaultKey, currentAdminVaultKey, currentUserIdVaultKey, currentAdminIdVaultKey)
import IHP.LoginSupport.Middleware (authMiddlewareWith, userIdMiddleware, adminIdMiddleware, parseSessionUUID)
import IHP.Controller.Session (sessionVaultKey)
import IHP.ModelSupport.Types (GetTableName, PrimaryKey, Id' (Id))
import qualified IHP.LoginSupport.Helper.Controller as Controller
import qualified IHP.LoginSupport.Helper.View as View
import qualified Data.Vault.Lazy as Vault
import qualified Network.Wai.Session as WaiSession

tests :: Spec
tests = do
    describe "Auth Middleware" do
        describe "user authentication" do
            it "populates Controller.currentUserOrNothing when user is authenticated" do
                let user = TestUser { id = "00000000-0000-0000-0000-000000000001" }
                let app = userMiddleware (Just user) $ \req respond -> do
                        let ?request = req
                        Controller.currentUserOrNothing `shouldBe` Just user
                        respond $ Wai.responseLBS status200 [] ""
                runSession (request defaultRequest >> pure ()) app

            it "Controller.currentUserOrNothing returns Nothing when not authenticated" do
                let app = userMiddleware Nothing $ \req respond -> do
                        let ?request = req
                        (Controller.currentUserOrNothing :: Maybe CurrentUserRecord) `shouldBe` Nothing
                        respond $ Wai.responseLBS status200 [] ""
                runSession (request defaultRequest >> pure ()) app

            it "populates View.currentUserOrNothing when user is authenticated" do
                let user = TestUser { id = "00000000-0000-0000-0000-000000000001" }
                let app = userMiddleware (Just user) $ \req respond -> do
                        let ?request = req
                        View.currentUserOrNothing `shouldBe` Just user
                        respond $ Wai.responseLBS status200 [] ""
                runSession (request defaultRequest >> pure ()) app

        describe "admin authentication" do
            it "populates Controller.currentAdminOrNothing when admin is authenticated" do
                let admin = TestAdmin { id = "00000000-0000-0000-0000-000000000002" }
                let app = adminMiddleware (Just admin) $ \req respond -> do
                        let ?request = req
                        Controller.currentAdminOrNothing `shouldBe` Just admin
                        respond $ Wai.responseLBS status200 [] ""
                runSession (request defaultRequest >> pure ()) app

            it "Controller.currentAdminOrNothing returns Nothing when not authenticated" do
                let app = adminMiddleware Nothing $ \req respond -> do
                        let ?request = req
                        (Controller.currentAdminOrNothing :: Maybe CurrentAdminRecord) `shouldBe` Nothing
                        respond $ Wai.responseLBS status200 [] ""
                runSession (request defaultRequest >> pure ()) app

        describe "middleware composition" do
            it "supports both user and admin simultaneously" do
                let user = TestUser { id = "00000000-0000-0000-0000-000000000001" }
                let admin = TestAdmin { id = "00000000-0000-0000-0000-000000000002" }
                let composed = userMiddleware (Just user) . adminMiddleware (Just admin)
                let app = composed $ \req respond -> do
                        let ?request = req
                        Controller.currentUserOrNothing `shouldBe` Just user
                        Controller.currentAdminOrNothing `shouldBe` Just admin
                        respond $ Wai.responseLBS status200 [] ""
                runSession (request defaultRequest >> pure ()) app

            it "user middleware does not affect admin lookup" do
                let user = TestUser { id = "00000000-0000-0000-0000-000000000001" }
                let app = userMiddleware (Just user) $ \req respond -> do
                        let ?request = req
                        (Controller.currentAdminOrNothing :: Maybe CurrentAdminRecord) `shouldBe` Nothing
                        respond $ Wai.responseLBS status200 [] ""
                runSession (request defaultRequest >> pure ()) app

    describe "parseSessionUUID" do
        it "parses a raw 36-byte UUID (new format)" do
            let uuid = "550e8400-e29b-41d4-a716-446655440000"
            parseSessionUUID uuid `shouldBe` UUID.fromASCIIBytes uuid

        it "parses a cereal-encoded 44-byte UUID (old format, backward compat)" do
            -- Old format: 8-byte cereal length prefix + 36-byte UUID ASCII
            let uuid = "550e8400-e29b-41d4-a716-446655440000"
            let cerealPrefix = BS.pack [0, 0, 0, 0, 0, 0, 0, 36] -- 8-byte big-endian length = 36
            let oldFormat = cerealPrefix <> uuid
            BS.length oldFormat `shouldBe` 44
            parseSessionUUID oldFormat `shouldBe` UUID.fromASCIIBytes uuid

        it "returns Nothing for empty bytes" do
            parseSessionUUID "" `shouldBe` Nothing

        it "returns Nothing for invalid bytes" do
            parseSessionUUID "not-a-uuid" `shouldBe` Nothing

        it "returns Nothing for random 44 bytes that don't contain a UUID" do
            let garbage = BS.replicate 44 0x41  -- 44 'A' bytes
            parseSessionUUID garbage `shouldBe` Nothing

    describe "userIdMiddleware" do
        it "populates currentUserIdVaultKey from session" do
            let uuid = "550e8400-e29b-41d4-a716-446655440000"
            let rawBytes = UUID.toASCIIBytes uuid
            let sessionMiddleware = mockSessionMiddleware "login.user" rawBytes
            let composed = sessionMiddleware . userIdMiddleware "login.user"
            let app = composed $ \req respond -> do
                    let ?request = req
                    Controller.currentUserIdOrNothing `shouldBe` Just (Id uuid)
                    respond $ Wai.responseLBS status200 [] ""
            runSession (request defaultRequest >> pure ()) app

        it "leaves currentUserOrNothing as Nothing (no fetch)" do
            let uuid = "550e8400-e29b-41d4-a716-446655440000"
            let rawBytes = UUID.toASCIIBytes uuid
            let sessionMiddleware = mockSessionMiddleware "login.user" rawBytes
            let composed = sessionMiddleware . userIdMiddleware "login.user"
            let app = composed $ \req respond -> do
                    let ?request = req
                    (Controller.currentUserOrNothing :: Maybe CurrentUserRecord) `shouldBe` Nothing
                    respond $ Wai.responseLBS status200 [] ""
            runSession (request defaultRequest >> pure ()) app

        it "handles cereal-encoded session values (backward compat)" do
            let uuid = "550e8400-e29b-41d4-a716-446655440000"
            let cerealPrefix = BS.pack [0, 0, 0, 0, 0, 0, 0, 36]
            let oldBytes = cerealPrefix <> UUID.toASCIIBytes uuid
            let sessionMiddleware = mockSessionMiddleware "login.user" oldBytes
            let composed = sessionMiddleware . userIdMiddleware "login.user"
            let app = composed $ \req respond -> do
                    let ?request = req
                    Controller.currentUserIdOrNothing `shouldBe` Just (Id uuid)
                    respond $ Wai.responseLBS status200 [] ""
            runSession (request defaultRequest >> pure ()) app

        it "returns Nothing for empty session" do
            let sessionMiddleware = mockEmptySessionMiddleware
            let composed = sessionMiddleware . userIdMiddleware "login.user"
            let app = composed $ \req respond -> do
                    let ?request = req
                    Controller.currentUserIdOrNothing `shouldBe` Nothing
                    respond $ Wai.responseLBS status200 [] ""
            runSession (request defaultRequest >> pure ()) app

    describe "adminIdMiddleware" do
        it "populates currentAdminIdVaultKey from session" do
            let uuid = "550e8400-e29b-41d4-a716-446655440000"
            let rawBytes = UUID.toASCIIBytes uuid
            let sessionMiddleware = mockSessionMiddleware "login.admin" rawBytes
            let composed = sessionMiddleware . adminIdMiddleware "login.admin"
            let app = composed $ \req respond -> do
                    let ?request = req
                    Controller.currentAdminIdOrNothing `shouldBe` Just (Id uuid)
                    respond $ Wai.responseLBS status200 [] ""
            runSession (request defaultRequest >> pure ()) app

        it "admin middleware does not affect user id lookup" do
            let uuid = "550e8400-e29b-41d4-a716-446655440000"
            let rawBytes = UUID.toASCIIBytes uuid
            let sessionMiddleware = mockSessionMiddleware "login.admin" rawBytes
            let composed = sessionMiddleware . adminIdMiddleware "login.admin"
            let app = composed $ \req respond -> do
                    let ?request = req
                    Controller.currentUserIdOrNothing `shouldBe` Nothing
                    respond $ Wai.responseLBS status200 [] ""
            runSession (request defaultRequest >> pure ()) app

-- | Mock user type for tests
data TestUser = TestUser { id :: UUID }
    deriving (Eq, Show)

-- | Mock admin type for tests
data TestAdmin = TestAdmin { id :: UUID }
    deriving (Eq, Show)

type instance CurrentUserRecord = TestUser
type instance CurrentAdminRecord = TestAdmin

type instance GetTableName TestUser = "test_users"
type instance PrimaryKey "test_users" = UUID
type instance GetTableName TestAdmin = "test_admins"
type instance PrimaryKey "test_admins" = UUID

-- | Middleware that stores a known user, using the same code path as the real middleware.
userMiddleware :: Maybe TestUser -> Wai.Middleware
userMiddleware user = authMiddlewareWith currentUserVaultKey (\_ -> pure user)

-- | Middleware that stores a known admin, using the same code path as the real middleware.
adminMiddleware :: Maybe TestAdmin -> Wai.Middleware
adminMiddleware admin = authMiddlewareWith currentAdminVaultKey (\_ -> pure admin)

-- | Create a mock session middleware that returns a specific value for a specific key.
mockSessionMiddleware :: ByteString -> ByteString -> Wai.Middleware
mockSessionMiddleware targetKey value app req respond = do
    let lookupFn key = pure $ if key == targetKey then Just value else Nothing
    let insertFn _ _ = pure ()
    let session = (lookupFn, insertFn) :: WaiSession.Session IO ByteString ByteString
    let req' = req { Wai.vault = Vault.insert sessionVaultKey session (Wai.vault req) }
    app req' respond

-- | Create a mock session middleware that returns Nothing for all keys.
mockEmptySessionMiddleware :: Wai.Middleware
mockEmptySessionMiddleware app req respond = do
    let lookupFn _ = pure Nothing
    let insertFn _ _ = pure ()
    let session = (lookupFn, insertFn) :: WaiSession.Session IO ByteString ByteString
    let req' = req { Wai.vault = Vault.insert sessionVaultKey session (Wai.vault req) }
    app req' respond
