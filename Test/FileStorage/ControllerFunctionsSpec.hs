module Test.FileStorage.ControllerFunctionsSpec where

import Test.Hspec
import IHP.Prelude
import IHP.FileStorage.ControllerFunctions
import IHP.Controller.Context
import IHP.FrameworkConfig
import IHP.ModelSupport
import IHP.FileStorage.Types
import System.IO.Temp (withSystemTempDirectory)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import Data.Default (def)
import Data.Time.Clock (getCurrentTime, addUTCTime)
import Network.Wai as Wai (defaultRequest)
import Network.Wai.Parse (FileInfo(..))
import IHP.Controller.RequestContext
import IHP.FileStorage.Config
import qualified IHP.FrameworkConfig as Config


tests :: Spec
tests = describe "IHP.FileStorage.ControllerFunctions" $ do

    let config :: ConfigBuilder
        config = do
            initStaticDirStorage

    let withFrameworkConfig = IHP.FrameworkConfig.withFrameworkConfig config


    describe "storeFileWithOptions" $ do
        it "returns the objectPath without the baseUrl" $ do
            withSystemTempDirectory "ihp-test" $ \tempDir -> do
                withFrameworkConfig \frameworkConfig -> do
                    context <- createControllerContext frameworkConfig
                    let ?context = context

                    let fileInfo = FileInfo
                            { fileName = "test.txt"
                            , fileContentType = "text/plain"
                            , fileContent = "Hello, world!"
                            }

                    result <- storeFile fileInfo "Test.FileStorage.ControllerFunctionsSpec"

                    result.url `shouldBe` "Test.FileStorage.ControllerFunctionsSpec/test.txt"

    describe "createTemporaryDownloadUrlFromPath" $ do
        it "returns baseUrl concatenated with objectPath when objectPath does not start with http:// or https://" $ do
            withFrameworkConfig \frameworkConfig -> do
                context <- createControllerContext frameworkConfig
                let ?context = context
                let objectPath = "static/test.txt"
                temporaryDownloadUrl <- createTemporaryDownloadUrlFromPath objectPath

                temporaryDownloadUrl.url `shouldBe` "http://localhost:8000/static/test.txt"

        it "returns '/' concatenated with objectPath when objectPath starts with 'http://' or 'https://'" $ do
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




