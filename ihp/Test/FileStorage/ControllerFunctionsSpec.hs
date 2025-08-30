module Test.FileStorage.ControllerFunctionsSpec where

import Test.Hspec
import IHP.Prelude
import IHP.FileStorage.ControllerFunctions
import IHP.Controller.Context
import IHP.FrameworkConfig
import Network.Wai as Wai (defaultRequest)
import Network.Wai.Parse (FileInfo(..))
import IHP.Controller.RequestContext
import IHP.FileStorage.Types
import IHP.FileStorage.Config

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
        requestBody = FormBody { params = [], files = [] }
        request = Wai.defaultRequest
        requestContext = RequestContext { request, respond = error "respond", requestBody, frameworkConfig = frameworkConfig }
    let ?requestContext = requestContext
    newControllerContext
