{-|
Module: Test.Controller.FileUploadBackendSpec
Tests for the app-wide file upload backend (@option TempFileUploads@).
-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.Controller.FileUploadBackendSpec where
import ClassyPrelude
import Test.Hspec
import IHP.Test.Mocking
import IHP.Prelude
import IHP.Environment
import IHP.RouterSupport hiding (get)
import IHP.FrameworkConfig
import IHP.EnvVar (EnvVarReader (..))
import Data.Either (isLeft)
import IHP.ViewPrelude hiding (param)
import IHP.ControllerPrelude hiding (get, request)
import Network.Wai.Test
import Network.HTTP.Types (methodPost, hContentType)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import System.Directory (doesFileExist)

data WebApplication = WebApplication deriving (Eq, Show, Data)

data UploadsController
    = UploadAction
  deriving (Eq, Show, Data)

instance Controller UploadsController where
    action UploadAction = renderUploadInfo

-- | Renders the temp file path, its content on disk, the file content and the title param
renderUploadInfo :: (?request :: Request, ?respond :: Respond) => IO ResponseReceived
renderUploadInfo = do
    tempFile <- tempFileOrNothing "file"
    let tempPath = maybe "none" (.fileContent) tempFile
    onDisk <- maybe (pure "") (LBS.readFile . (.fileContent)) tempFile
    let content = maybe "" (.fileContent) (fileOrNothing "file")
    let title :: Text = param "title"
    renderPlain (cs tempPath <> "|" <> onDisk <> "|" <> content <> "|" <> cs title)

instance AutoRoute UploadsController

instance FrontController WebApplication where
  controllers = [ parseRoute @UploadsController ]

instance InitControllerContext WebApplication where
  initContext = pure ()

instance FrontController RootApplication where
    controllers = [ mountFrontController WebApplication ]

config = do
    option Development
    option (AppPort 8000)

tempFileConfig = do
    config
    option TempFileUploads

postMultipart :: ByteString -> Session SResponse
postMultipart url = srequest $ SRequest req body
  where
    req = setPath defaultRequest
        { requestMethod = methodPost
        , requestHeaders = [(hContentType, "multipart/form-data; boundary=BOUNDARY")]
        } url
    body = LBS.concat
        [ "--BOUNDARY\r\n"
        , "Content-Disposition: form-data; name=\"title\"\r\n\r\n"
        , "Hello\r\n"
        , "--BOUNDARY\r\n"
        , "Content-Disposition: form-data; name=\"file\"; filename=\"video.mp4\"\r\n"
        , "Content-Type: video/mp4\r\n\r\n"
        , "file-content\r\n"
        , "--BOUNDARY--\r\n"
        ]

tests :: Spec
tests = describe "fileUploadBackend" do
    describe "IHP_FILE_UPLOAD_MEMORY_THRESHOLD" do
        it "reads a size in bytes as TempFileUploadsAbove" do
            envStringToValue "1048576" `shouldBe` Right (TempFileUploadsAbove 1048576)
            envStringToValue "0" `shouldBe` Right (TempFileUploadsAbove 0)

        it "rejects a negative or non-numeric value" do
            (envStringToValue "-1" :: Either Text FileUploadBackend) `shouldSatisfy` isLeft
            (envStringToValue "1MB" :: Either Text FileUploadBackend) `shouldSatisfy` isLeft

    aroundAll (withMockContextAndApp WebApplication config) do
        it "keeps uploads in memory by default, writing a temp file on tempFileOrNothing" $ withContextAndApp \application -> do
            response <- runSession (postMultipart "test/Upload") application
            expectTempFileRemoved response

    aroundAll (withMockContextAndApp WebApplication tempFileConfig) do
        it "writes uploads to a temp file with option TempFileUploads, and removes it after the request" $ withContextAndApp \application -> do
            response <- runSession (postMultipart "test/Upload") application
            expectTempFileRemoved response

-- | The action got a temp file with the uploaded content, removed after the request
expectTempFileRemoved :: SResponse -> IO ()
expectTempFileRemoved response =
    case Text.splitOn "|" (cs (simpleBody response)) of
        [tempPath, onDisk, content, title] -> do
            tempPath `shouldNotBe` "none"
            (onDisk, content, title) `shouldBe` ("file-content", "file-content", "Hello")
            doesFileExist (cs tempPath) >>= (`shouldBe` False)
        _ -> expectationFailure ("unexpected response: " <> cs (simpleBody response))
