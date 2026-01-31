{-|
Module: Test.LoginSupport.AuthVaultSpec
Copyright: (c) digitally induced GmbH, 2025
-}
module Test.LoginSupport.AuthVaultSpec where

import IHP.Prelude
import Test.Hspec

import qualified Data.Vault.Lazy as Vault
import qualified Network.Wai as Wai
import Network.Wai.Test (defaultRequest)
import IHP.LoginSupport.Types (CurrentUserRecord, CurrentAdminRecord, currentUserVaultKey, currentAdminVaultKey, lookupAuthVault)
import IHP.Controller.Context
import IHP.RequestVault (requestBodyVaultKey)
import Wai.Request.Params.Middleware (RequestBody (..))
import qualified IHP.LoginSupport.Helper.Controller as Controller
import qualified IHP.LoginSupport.Helper.View as View

tests :: Spec
tests = do
    describe "IHP.LoginSupport.Types" do
        describe "lookupAuthVault" do
            it "returns Nothing when vault is empty" do
                let request = defaultRequest
                lookupAuthVault currentUserVaultKey request `shouldBe` (Nothing :: Maybe CurrentUserRecord)

            it "returns Just user when vault has a user stored" do
                let userId = "00000000-0000-0000-0000-000000000001"
                let user = TestUser { id = userId }
                let request = defaultRequest
                        { Wai.vault = Vault.insert currentUserVaultKey (Just user) (Wai.vault defaultRequest) }
                lookupAuthVault currentUserVaultKey request `shouldBe` Just user

            it "returns Nothing when vault stores Nothing (logged-out state)" do
                let request = defaultRequest
                        { Wai.vault = Vault.insert currentUserVaultKey (Nothing :: Maybe CurrentUserRecord) (Wai.vault defaultRequest) }
                lookupAuthVault currentUserVaultKey request `shouldBe` Nothing

            it "returns Nothing when looking up admin key but only user is stored" do
                let userId = "00000000-0000-0000-0000-000000000001"
                let user = TestUser { id = userId }
                let request = defaultRequest
                        { Wai.vault = Vault.insert currentUserVaultKey (Just user) (Wai.vault defaultRequest) }
                lookupAuthVault currentAdminVaultKey request `shouldBe` (Nothing :: Maybe CurrentAdminRecord)

            it "supports both user and admin in the same vault simultaneously" do
                let userId = "00000000-0000-0000-0000-000000000001"
                let adminId = "00000000-0000-0000-0000-000000000002"
                let user = TestUser { id = userId }
                let admin = TestAdmin { id = adminId }
                let v = Vault.insert currentUserVaultKey (Just user)
                      $ Vault.insert currentAdminVaultKey (Just admin)
                      $ Wai.vault defaultRequest
                let request = defaultRequest { Wai.vault = v }
                lookupAuthVault currentUserVaultKey request `shouldBe` Just user
                lookupAuthVault currentAdminVaultKey request `shouldBe` Just admin

    describe "IHP.LoginSupport.Helper.Controller" do
        describe "currentUserOrNothing" do
            it "returns Nothing when no user in vault" do
                context <- createControllerContext defaultRequest
                let ?context = context
                (Controller.currentUserOrNothing :: Maybe CurrentUserRecord) `shouldBe` Nothing

            it "returns Just user when user is populated in vault" do
                let userId = "00000000-0000-0000-0000-000000000001"
                let user = TestUser { id = userId }
                let request = defaultRequest
                        { Wai.vault = Vault.insert currentUserVaultKey (Just user) (Wai.vault defaultRequest) }
                context <- createControllerContext request
                let ?context = context
                Controller.currentUserOrNothing `shouldBe` Just user

        describe "currentAdminOrNothing" do
            it "returns Nothing when no admin in vault" do
                context <- createControllerContext defaultRequest
                let ?context = context
                (Controller.currentAdminOrNothing :: Maybe CurrentAdminRecord) `shouldBe` Nothing

            it "returns Just admin when admin is populated in vault" do
                let adminId = "00000000-0000-0000-0000-000000000002"
                let admin = TestAdmin { id = adminId }
                let request = defaultRequest
                        { Wai.vault = Vault.insert currentAdminVaultKey (Just admin) (Wai.vault defaultRequest) }
                context <- createControllerContext request
                let ?context = context
                Controller.currentAdminOrNothing `shouldBe` Just admin

    describe "IHP.LoginSupport.Helper.View" do
        describe "currentUserOrNothing" do
            it "returns Nothing when no user in vault" do
                context <- createControllerContext defaultRequest
                let ?context = context
                (View.currentUserOrNothing :: Maybe CurrentUserRecord) `shouldBe` Nothing

            it "returns Just user when user is populated in vault" do
                let userId = "00000000-0000-0000-0000-000000000001"
                let user = TestUser { id = userId }
                let request = defaultRequest
                        { Wai.vault = Vault.insert currentUserVaultKey (Just user) (Wai.vault defaultRequest) }
                context <- createControllerContext request
                let ?context = context
                View.currentUserOrNothing `shouldBe` Just user

-- | Mock user type for tests
data TestUser = TestUser { id :: UUID }
    deriving (Eq, Show)

-- | Mock admin type for tests
data TestAdmin = TestAdmin { id :: UUID }
    deriving (Eq, Show)

-- | Bind the type families for test usage
type instance CurrentUserRecord = TestUser
type instance CurrentAdminRecord = TestAdmin

-- | Create a controller context from a WAI request, following the CookieSpec pattern.
createControllerContext :: Wai.Request -> IO ControllerContext
createControllerContext request = do
    let requestBody = FormBody { params = [], files = [] }
    let requestWithBody = request { Wai.vault = Vault.insert requestBodyVaultKey requestBody (Wai.vault request) }
    let ?request = requestWithBody
    newControllerContext
