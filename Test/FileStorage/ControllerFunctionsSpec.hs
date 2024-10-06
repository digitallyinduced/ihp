{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}

module Test.FileStorage.ControllerFunctionsSpec (spec) where

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
import IHP.Controller.RequestContext (RequestContext (..))
import Network.Wai (defaultRequest)

spec :: Spec
spec = describe "IHP.FileStorage.ControllerFunctions" $ do
    describe "storeFileWithOptions" $ do
        it "returns the objectPath without the baseUrl" $ do
            withSystemTempDirectory "ihp-test" $ \tempDir -> do
                context <- newControllerContext
                let ?context = context

                let fileInfo = FileInfo
                        { fileName = "test.txt"
                        , contentType = "text/plain"
                        , fileContent = "Hello, world!"
                        }
                let options = def { objectName = Just "test.txt" }

                result <- storeFileWithOptions fileInfo options

                result `shouldBe` "static/test.txt"

    describe "createTemporaryDownloadUrlFromPathWithExpiredAt" $ do
        it "returns baseUrl concatenated with objectPath when objectPath does not start with http:// or https://" $ do
            context <- newControllerContext
            let objectPath = "static/test.txt"
            temporaryDownloadUrl <- createTemporaryDownloadUrlFromPathWithExpiredAt 3600 objectPath

            temporaryDownloadUrl.url `shouldBe` "http://localhost:8000/static/test.txt"

        it "returns '/' concatenated with objectPath when objectPath starts with 'http://' or 'https://'" $ do
            context <- newControllerContext
            let objectPath = "https://example.com/static/test.txt"
            temporaryDownloadUrl <- createTemporaryDownloadUrlFromPathWithExpiredAt 3600 objectPath

            temporaryDownloadUrl.url `shouldBe` "https://example.com/static/test.txt"
