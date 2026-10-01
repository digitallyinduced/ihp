{-|
Module: Test.Controller.FileUploadBackendSpec
Tests for choosing the file upload backend per controller action.
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
import IHP.ViewPrelude hiding (param)
import IHP.ControllerPrelude hiding (get, request)
import Network.Wai.Test
import Network.HTTP.Types (methodPost, hContentType)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import System.Directory (doesFileExist)

data WebApplication = WebApplication deriving (Eq, Show, Data)

data UploadsController
    = UploadInMemoryAction
    | UploadToDiskAction
  deriving (Eq, Show, Data)

instance Controller UploadsController where
    fileUploadBackend UploadToDiskAction = Just TempFileUploads
    fileUploadBackend _ = Nothing

    action UploadInMemoryAction = renderUploadInfo
    action UploadToDiskAction = renderUploadInfo

-- | Renders the temp file path (if any), the file content and the title param
renderUploadInfo :: (?request :: Request, ?respond :: Respond) => IO ResponseReceived
renderUploadInfo = do
    let tempPath = maybe "none" (.fileContent) (tempFileOrNothing "file")
    let content = maybe "" (.fileContent) (fileOrNothing "file")
    let title :: Text = param "title"
    renderPlain (cs tempPath <> "|" <> content <> "|" <> cs title)

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
tests = aroundAll (withMockContextAndApp WebApplication config) do
    describe "fileUploadBackend" $ do
        it "keeps uploads in memory by default" $ withContextAndApp \application -> do
            response <- runSession (postMultipart "test/UploadInMemory") application
            simpleBody response `shouldBe` "none|file-content|Hello"

        it "writes uploads to a temp file for an action that chooses TempFileUploads, and removes it after the request" $ withContextAndApp \application -> do
            response <- runSession (postMultipart "test/UploadToDisk") application
            case Text.splitOn "|" (cs (simpleBody response)) of
                [tempPath, content, title] -> do
                    tempPath `shouldNotBe` "none"
                    (content, title) `shouldBe` ("file-content", "Hello")
                    doesFileExist (cs tempPath) >>= (`shouldBe` False)
                _ -> expectationFailure ("unexpected response: " <> cs (simpleBody response))
