{-|
Module: Test.ServerSpec
Description: Tests for the dev-mode connection handling in IHP.Server
-}
module Test.ServerSpec where

import Test.Hspec
import IHP.Prelude
import IHP.Server (closeConnectionAfterResponse)
import Network.Wai
import Network.Wai.Internal (ResponseReceived (..))
import Network.Wai.Test
import Network.HTTP.Types

tests :: Spec
tests = describe "IHP.Server.closeConnectionAfterResponse" do
    let plainApp :: Application
        plainApp _ respond = respond (responseLBS status200 [(hContentType, "text/plain")] "hello")

    it "tells the client to close the connection after the response" do
        response <- runSession (request defaultRequest) (closeConnectionAfterResponse plainApp)
        simpleStatus response `shouldBe` status200
        lookup hConnection (simpleHeaders response) `shouldBe` Just "close"
        lookup hContentType (simpleHeaders response) `shouldBe` Just "text/plain"

    it "replaces a Connection header the app set itself" do
        let keepAliveApp :: Application
            keepAliveApp _ respond = respond (responseLBS status200 [(hConnection, "keep-alive")] "")
        response <- runSession (request defaultRequest) (closeConnectionAfterResponse keepAliveApp)
        filter ((== hConnection) . fst) (simpleHeaders response) `shouldBe` [(hConnection, "close")]

    it "leaves a raw (WebSocket upgrade) response untouched" do
        -- A raw response keeps its connection open for the upgrade, so it must not
        -- be told to close. 'mapResponseHeaders' skips 'ResponseRaw'; this pins that.
        let fallback = responseLBS status426 [] "upgrade required"
            rawApp :: Application
            rawApp _ respond = respond (responseRaw (\_ _ -> pure ()) fallback)
        seen <- newIORef Nothing
        let capture response = do
                writeIORef seen (Just (responseHeaders response))
                pure ResponseReceived
        _ <- closeConnectionAfterResponse rawApp defaultRequest capture
        readIORef seen `shouldReturn` Just []
