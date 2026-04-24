{-|
Module: Test.Router.MiddlewareSpec
Copyright: (c) digitally induced GmbH, 2026
-}
module Test.Router.MiddlewareSpec where

import Test.Hspec
import IHP.Prelude
import IHP.Router.Trie
import IHP.Router.Middleware
import Network.HTTP.Types (status200, status404, status405)
import Network.HTTP.Types.Method (StdMethod (..))
import Network.HTTP.Types.Method (renderStdMethod)
import Network.Wai (Application, Response, responseLBS, defaultRequest, requestMethod, rawPathInfo, responseStatus, responseHeaders)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import Data.IORef (newIORef, readIORef, writeIORef)

-- | Dummy handler that responds 200 OK with a tag.
tagHandler :: LBS.ByteString -> WaiHandler
tagHandler tag _captures _req respond = respond (responseLBS status200 [] tag)

-- | Fallback application: always 404 with a marker body.
fallbackApp :: Application
fallbackApp _req respond = respond (responseLBS status404 [] "FALLBACK")

-- | Run the middleware for a synthetic request, returning the outgoing Response.
runWith :: RouteTrie -> StdMethod -> BS.ByteString -> IO Response
runWith trie method path = do
    let req = defaultRequest
            { requestMethod = renderStdMethod method
            , rawPathInfo = path
            }
    ref <- newIORef Nothing
    let respond r = do
            writeIORef ref (Just r)
            pure (error "unused responseReceived")
    _ <- routeTrieMiddleware trie fallbackApp req respond
    saved <- readIORef ref
    case saved of
        Just r  -> pure r
        Nothing -> error "middleware did not call respond"

allowHeader :: Response -> Maybe BS.ByteString
allowHeader r = List.lookup "Allow" (responseHeaders r)

tests = do
    describe "IHP.Router.Middleware" do
        let trie = foldr (\(m, p, tag) t -> insertRoute p m (tagHandler tag) t)
                emptyTrie
                [ (GET, [LiteralSeg "posts"], "list")
                , (POST, [LiteralSeg "posts"], "create")
                , (GET, [LiteralSeg "posts", LiteralSeg "new"], "new")
                ]

        it "returns 200 for a matched route" do
            resp <- runWith trie GET "/posts"
            responseStatus resp `shouldBe` status200

        it "returns 404 (fallback) for an unmatched path" do
            resp <- runWith trie GET "/missing"
            responseStatus resp `shouldBe` status404

        it "returns 405 with Allow header for a matched path but wrong method" do
            resp <- runWith trie PATCH "/posts"
            responseStatus resp `shouldBe` status405
            case allowHeader resp of
                Just hdr -> do
                    -- Should list GET and POST (order not specified)
                    ("GET" `BS.isInfixOf` hdr) `shouldBe` True
                    ("POST" `BS.isInfixOf` hdr) `shouldBe` True
                Nothing -> expectationFailure "Allow header missing"
