module Main where

import Test.Hspec
import Network.Wai
import Network.Wai.Test
import Network.HTTP.Types
import Network.Wai.Middleware.EarlyReturn

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "earlyReturnMiddleware" $ do
        it "returns the early response when earlyReturn is called" $ do
            let earlyResponse = responseLBS status200 [] "early"
            let lateResponse = responseLBS status500 [] "should not reach"
            let app = earlyReturnMiddleware $ \_request respond -> do
                    earlyReturn (respond earlyResponse)
                    respond lateResponse

            response <- runSession (request defaultRequest) app

            simpleStatus response `shouldBe` status200
            simpleBody response `shouldBe` "early"

        it "passes through normal responses" $ do
            let normalResponse = responseLBS status200 [] "normal"
            let app = earlyReturnMiddleware $ \_request respond ->
                    respond normalResponse

            response <- runSession (request defaultRequest) app

            simpleStatus response `shouldBe` status200
            simpleBody response `shouldBe` "normal"
