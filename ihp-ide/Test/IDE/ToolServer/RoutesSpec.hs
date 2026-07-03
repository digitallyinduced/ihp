{-|
Module: IDE.ToolServer.RoutesSpec
Tests for the ToolServer route table.

Regression test for https://github.com/digitallyinduced/ihp/issues/2743:
the codegen views submit their "Preview" forms (and option toggles) via
@\<form method="POST" action={New…Action}>@, so the @New*@ routes must accept
POST in addition to GET. When the routes were migrated from AutoRoute (which
allowed @[GET, POST, HEAD]@ for @New*@ actions) to the routes DSL, they were
declared GET-only and every codegen preview failed with @405 Method Not Allowed@.
-}
module IDE.ToolServer.RoutesSpec where

import IHP.Prelude
import Test.Hspec
import qualified Data.List as List
import Network.Wai (defaultRequest)
import Network.HTTP.Types.Method (StdMethod (..))
import IHP.RouterSupport (FrontController (..), ControllerRoute (..))
import qualified IHP.Router.Trie as Trie
import IHP.IDE.ToolServer ()
import IHP.IDE.ToolServer.Types (ToolServerApplication)

-- | The merged route trie of the ToolServer, as built by 'frontControllerToWAIApp'.
--
-- The handlers close over @?application@/@?respond@, but route matching never
-- forces them, so error placeholders are fine here.
toolServerTrie :: Trie.RouteTrie
toolServerTrie =
    let ?application = error "?application should not be forced during route matching" :: ToolServerApplication
        ?request = defaultRequest
        ?respond = error "?respond should not be forced during route matching"
    in List.foldl' Trie.mergeTrie Trie.emptyTrie [fragment | ControllerRouteTrie fragment <- controllers]

-- | Route lookup result reduced to a comparable description.
matchResult :: StdMethod -> ByteString -> Text
matchResult method path =
    case Trie.lookupTrie toolServerTrie method (Trie.splitPath path) of
        Trie.Matched _ _ -> "Matched"
        Trie.MethodNotAllowed allowed -> "MethodNotAllowed " <> tshow allowed
        Trie.NotMatched -> "NotMatched"

tests :: Spec
tests = do
    describe "ToolServer routes" do
        -- These actions render the codegen preview: reached via GET links from
        -- the Generators page, and via POST from their own preview/option forms
        let codegenPreviewPaths :: [ByteString]
            codegenPreviewPaths =
                [ "/NewController"
                , "/NewScript"
                , "/NewView"
                , "/NewMail"
                , "/NewAction"
                , "/NewApplication"
                , "/NewJob"
                , "/NewMigration"
                ]

        forEach codegenPreviewPaths \path -> do
            it ("routes GET " <> cs path) do
                matchResult GET path `shouldBe` "Matched"
            it ("routes POST " <> cs path) do
                matchResult POST path `shouldBe` "Matched"
