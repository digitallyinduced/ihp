module Test.AutoRoute.Util where

import IHP.Prelude
import Network.Wai (requestMethod, requestHeaders)
import Network.Wai.Test (Session, SResponse(..), SRequest(..), request, srequest, defaultRequest, setPath)
import Network.HTTP.Types (methodGet, methodPost, hContentType, renderSimpleQuery)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Test.Hspec (expectationFailure)

testGet :: ByteString -> Session SResponse
testGet url = request $ setPath defaultRequest { requestMethod = methodGet } url

testPost :: ByteString -> Session SResponse
testPost url = request $ setPath defaultRequest { requestMethod = methodPost } url

-- | POST a form-encoded body to a URL.
testPostForm :: ByteString -> [(ByteString, ByteString)] -> Session SResponse
testPostForm url params = srequest $ SRequest req body
  where
    req = setPath defaultRequest
        { requestMethod = methodPost
        , requestHeaders = [(hContentType, "application/x-www-form-urlencoded")]
        } url
    body = cs $ renderSimpleQuery False params

-- | POST a JSON body to a URL.
testPostJSON :: ByteString -> LBS.ByteString -> Session SResponse
testPostJSON url body = srequest $ SRequest req body
  where
    req = setPath defaultRequest
        { requestMethod = methodPost
        , requestHeaders = [(hContentType, "application/json")]
        } url

-- | Assert that the response body does NOT contain the given substring.
assertBodyNotContains :: ByteString -> SResponse -> Session ()
assertBodyNotContains needle SResponse { simpleBody }
    | needle `BS.isInfixOf` LBS.toStrict simpleBody =
        liftIO $ expectationFailure $ cs @Text $ "Expected body to not contain " <> tshow needle <> ", but it did"
    | otherwise = pure ()
