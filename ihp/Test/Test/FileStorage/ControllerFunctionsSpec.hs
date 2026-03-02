module Test.FileStorage.ControllerFunctionsSpec where

import Test.Hspec
import IHP.Prelude
import IHP.FileStorage.ControllerFunctions
import IHP.Controller.Context
import IHP.FrameworkConfig
import Network.Wai as Wai (defaultRequest)
import qualified Network.Wai as Wai
import Network.Wai.Parse (FileInfo(..))
import Wai.Request.Params.Middleware (RequestBody (..))
import IHP.FileStorage.Types
import IHP.FileStorage.Config
import qualified Data.Vault.Lazy as Vault
import qualified IHP.RequestVault

tests :: Spec
tests = describe "IHP.FileStorage.ControllerFunctions" $ do

    let config :: ConfigBuilder
        config = do
            initStaticDirStorage

    let withFrameworkConfig = IHP.FrameworkConfig.withFrameworkConfig config

    describe "storeFileWithOptions" $ do
        it "returns the objectPath without the baseUrl" $ do
            withFrameworkConfig \frameworkConfig -> do
                context <- createControllerContext frameworkConfig
                let ?context = context

                let fileInfo = FileInfo
                        { fileName = "test.txt"
                        , fileContentType = "text/plain"
                        , fileContent = "Hello, world!"
                        }

                -- We pass the UUID that will be used as the filename, so we can easily assert the objectPath.
                let options :: StoreFileOptions = def
                        { fileName = Just "4c55dac2-e411-45ac-aa10-b957b01221df"
                        , directory = "Test.FileStorage.ControllerFunctionsSpec"
                        }

                result <- storeFileWithOptions fileInfo options

                result.url `shouldBe` ("/Test.FileStorage.ControllerFunctionsSpec/4c55dac2-e411-45ac-aa10-b957b01221df")

    describe "contentDispositionAttachmentAndFileName" $ do
        let fileInfoWith name = FileInfo { fileName = name, fileContentType = "application/octet-stream", fileContent = "" }

        it "produces RFC 5987 encoded header for a plain ASCII filename" $ do
            result <- contentDispositionAttachmentAndFileName (fileInfoWith "report.pdf")
            result `shouldBe` Just "attachment; filename*=UTF-8''report.pdf"

        it "percent-encodes spaces in the filename" $ do
            result <- contentDispositionAttachmentAndFileName (fileInfoWith "my report.pdf")
            result `shouldBe` Just "attachment; filename*=UTF-8''my%20report.pdf"

        it "percent-encodes special characters in the filename" $ do
            result <- contentDispositionAttachmentAndFileName (fileInfoWith "file (final).pdf")
            result `shouldBe` Just "attachment; filename*=UTF-8''file%20%28final%29.pdf"

        it "percent-encodes non-ASCII characters in the filename" $ do
            result <- contentDispositionAttachmentAndFileName (fileInfoWith (cs ("ファイル.pdf" :: Text)))
            result `shouldBe` Just "attachment; filename*=UTF-8''%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB.pdf"

    describe "createTemporaryDownloadUrlFromPath" $ do
        it "returns baseUrl concatenated with objectPath when objectPath does not start with http:// or https://" $ do
            withFrameworkConfig \frameworkConfig -> do
                context <- createControllerContext frameworkConfig
                let ?context = context
                let objectPath = "static/test.txt"
                temporaryDownloadUrl <- createTemporaryDownloadUrlFromPath objectPath

                temporaryDownloadUrl.url `shouldBe` "http://localhost:8000/static/test.txt"

        it "returns baseUrl concatenated with objectPath without double slash when objectPath starts with '/'" $ do
            withFrameworkConfig \frameworkConfig -> do
                context <- createControllerContext frameworkConfig
                let ?context = context
                let objectPath = "/static/test.txt"
                temporaryDownloadUrl <- createTemporaryDownloadUrlFromPath objectPath

                temporaryDownloadUrl.url `shouldBe` "http://localhost:8000/static/test.txt"

        it "returns the objectPath when objectPath starts with 'http://' or 'https://'" $ do
            withFrameworkConfig \frameworkConfig -> do
                context <- createControllerContext frameworkConfig
                let ?context = context
                let objectPath = "https://example.com/static/test.txt"
                temporaryDownloadUrl <- createTemporaryDownloadUrlFromPath objectPath

                temporaryDownloadUrl.url `shouldBe` "https://example.com/static/test.txt"

createControllerContext frameworkConfig = do
    let
        requestBody = FormBody { params = [], files = [], rawPayload = "" }
        request = Wai.defaultRequest { Wai.vault = Vault.insert IHP.RequestVault.frameworkConfigVaultKey frameworkConfig
                                                 $ Vault.insert IHP.RequestVault.requestBodyVaultKey requestBody Vault.empty }
    let ?request = request
    newControllerContext
