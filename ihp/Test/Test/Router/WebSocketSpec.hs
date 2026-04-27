{-|
Module: Test.Router.WebSocketSpec
Copyright: (c) digitally induced GmbH, 2026

Verifies that @[routes|…|]@ blocks can declare WebSocket routes via the
@WS@ keyword. The splice emits a top-level binding that mixes
'parseRoute @Ctrl' (HTTP routes) with 'webSocketRoute @T \"\/path\"'
(WS routes); the user splats that binding into
'FrontController.controllers' the same way as for HTTP-only blocks.
-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Router.WebSocketSpec where

import Test.Hspec
import IHP.Prelude
import IHP.RouterSupport
import IHP.Router.DSL (routes)
import IHP.ControllerPrelude
import qualified IHP.WebSocket as WS
import qualified "ihp-router" IHP.Router.Trie as Trie
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Types.Method (StdMethod (..))
import qualified Network.Wai as Wai

-- HTTP fixture controller, used to verify WS and HTTP routes coexist
-- in the same DSL block.
data PingsCtrl = PingsIndexAction
    deriving (Eq, Show)

instance Controller PingsCtrl where
    action PingsIndexAction = renderPlain "pong"

-- WSApp fixtures. Both are nullary so 'WS.initialState' is just the
-- constructor itself — matching the static-path-only restriction of
-- the v1 DSL.
data ChatWSApp = ChatWSApp

instance WS.WSApp ChatWSApp where
    initialState = ChatWSApp
    run = do
        msg <- WS.receiveData @LBS.ByteString
        WS.sendTextData msg

data NotificationsWSApp = NotificationsWSApp

instance WS.WSApp NotificationsWSApp where
    initialState = NotificationsWSApp

data WSTestApplication = WSTestApplication deriving (Eq, Show, Data)

instance InitControllerContext WSTestApplication where
    initContext = pure ()

$(pure [])

-- A mixed block: one HTTP controller plus two WS apps. The splice
-- emits a 'CanRoute' instance for 'PingsCtrl' and skips it for the
-- WS types; the named binding contains 'parseRoute @PingsCtrl' plus
-- one 'webSocketRoute @T \"\/path\"' entry per WS route.
[routes|wsRoutes
GET /pings                 PingsIndexAction
WS  /chat                  ChatWSApp
WS  /notifications         NotificationsWSApp
|]

instance FrontController WSTestApplication where
    controllers = wsRoutes

tests = do
    describe "IHP.Router.DSL — WS route support" do
        it "HasPath still works for HTTP routes in mixed blocks" do
            pathTo PingsIndexAction `shouldBe` "/pings"

        it "wsRoutes fits the FrontController.controllers contract" do
            -- Compile-time proof: the 'instance FrontController
            -- WSTestApplication where controllers = wsRoutes' above
            -- type-checks only if 'wsRoutes' has the right shape and
            -- each WS-route entry resolves the 'WSApp' / 'Typeable'
            -- constraints the splice attaches to its signature.
            True `shouldBe` True

    describe "IHP.RouterSupport.webSocketRoute" do
        it "produces a trie-backed ControllerRoute that matches GET on the path" do
            -- 'webSocketRoute' is what the DSL emits in the named
            -- binding; verify it builds a 'ControllerRouteTrie' whose
            -- internal trie matches the WS path under GET (the
            -- handshake method) and 405s for other methods.
            let ?request = Wai.defaultRequest
                ?respond = \_ -> error "respond stub forced"
                ?application = WSTestApplication
            case webSocketRoute @ChatWSApp @WSTestApplication "/chat" of
                ControllerRouteTrie trie -> do
                    case Trie.lookupTrie trie GET (Trie.splitPath "/chat") of
                        Trie.Matched _ _ -> pure ()
                        other -> expectationFailure
                            ("expected Matched for GET /chat, got "
                                <> showLookup other)
                    case Trie.lookupTrie trie POST (Trie.splitPath "/chat") of
                        Trie.MethodNotAllowed [GET] -> pure ()
                        other -> expectationFailure
                            ("expected MethodNotAllowed [GET] for POST /chat, got "
                                <> showLookup other)
                    case Trie.lookupTrie trie GET (Trie.splitPath "/elsewhere") of
                        Trie.NotMatched -> pure ()
                        other -> expectationFailure
                            ("expected NotMatched for GET /elsewhere, got "
                                <> showLookup other)
                _ -> expectationFailure
                    "webSocketRoute returned a non-trie ControllerRoute"
  where
    showLookup :: Trie.LookupResult -> String
    showLookup (Trie.Matched _ caps) =
        "Matched (captures: " <> cs (tshow caps) <> ")"
    showLookup (Trie.MethodNotAllowed ms) =
        "MethodNotAllowed " <> cs (tshow ms)
    showLookup Trie.NotMatched = "NotMatched"
