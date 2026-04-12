module Test.Util where

import IHP.Prelude
import Network.Wai (requestMethod)
import Network.Wai.Test (Session, SResponse(..), request, defaultRequest, setPath)
import Network.HTTP.Types (methodGet, methodPost)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Test.Hspec (expectationFailure)

testGet :: ByteString -> Session SResponse
testGet url = request $ setPath defaultRequest { requestMethod = methodGet } url

testPost :: ByteString -> Session SResponse
testPost url = request $ setPath defaultRequest { requestMethod = methodPost } url

-- | Assert that the response body does NOT contain the given substring.
-- Complement to Network.Wai.Test.assertBodyContains.
assertBodyNotContains :: ByteString -> SResponse -> Session ()
assertBodyNotContains needle SResponse { simpleBody }
    | needle `BS.isInfixOf` LBS.toStrict simpleBody =
        liftIO $ expectationFailure $ cs @Text $ "Expected body to not contain " <> tshow needle <> ", but it did"
    | otherwise = pure ()
