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
import Network.Wai (defaultRequest)
import Network.Wai.Parse (FileInfo(..))

tests :: Spec
tests = describe "IHP.FileStorage.ControllerFunctions" $ do
    let ?requestContext = undefined
    describe "storeFileWithOptions" $ do
        it "returns the objectPath without the baseUrl" $ do
            withSystemTempDirectory "ihp-test" $ \tempDir -> do
                context <- newControllerContext
                let ?context = context

                let fileInfo = FileInfo
                        { fileName = "test.txt"
                        , fileContentType = "text/plain"
                        , fileContent = "Hello, world!"
                        }

                result <- storeFile fileInfo "Test.FileStorage.ControllerFunctionsSpec"

                result.url `shouldBe` "Test.FileStorage.ControllerFunctionsSpec/test.txt"

    describe "createTemporaryDownloadUrlFromPathWithExpiredAt" $ do
        it "returns baseUrl concatenated with objectPath when objectPath does not start with http:// or https://" $ do
            context <- newControllerContext
            let ?context = context
            let objectPath = "static/test.txt"
            temporaryDownloadUrl <- createTemporaryDownloadUrlFromPath objectPath

            temporaryDownloadUrl.url `shouldBe` "http://localhost:8000/static/test.txt"

        it "returns '/' concatenated with objectPath when objectPath starts with 'http://' or 'https://'" $ do
            context <- newControllerContext
            let ?context = context
            let objectPath = "https://example.com/static/test.txt"
            temporaryDownloadUrl <- createTemporaryDownloadUrlFromPath objectPath

            temporaryDownloadUrl.url `shouldBe` "https://example.com/static/test.txt"
