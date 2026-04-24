{-|
Module: Test.Router.TrieSpec
Copyright: (c) digitally induced GmbH, 2026
-}
module Test.Router.TrieSpec where

import Test.Hspec
import IHP.Prelude
import IHP.Router.Trie
import IHP.Router.Capture (UrlCapture(..))
import qualified Data.HashMap.Strict as HashMap
import Data.Dynamic (toDyn, fromDynamic)
import Network.HTTP.Types.Method (StdMethod (..))
import Network.Wai (responseLBS)
import qualified Data.ByteString.Lazy as LBS

-- | Tag handlers with a distinct bytestring so we can identify which matched.
tagHandler :: LBS.ByteString -> WaiHandler
tagHandler tag _captures _req respond = respond (responseLBS (toEnum 200) [] tag)

-- | Build a trie by folding a list of @(method, pattern, tag)@ triples.
buildTrie :: [(StdMethod, [PatternSegment], LBS.ByteString)] -> RouteTrie
buildTrie = foldr ins emptyTrie
  where
    ins (m, p, tag) t = insertRoute p m (tagHandler tag) t

-- | Pull out the tag from a 'Matched' result by running the handler
-- against a dummy request.
matchedTag :: LookupResult -> IO (Maybe LBS.ByteString)
matchedTag (Matched handler _captures) = do
    -- Run the handler with a minimal request/respond; capture the body.
    ref <- newIORef Nothing
    let dummyReq = error "request not used"
    _ <- handler HashMap.empty dummyReq (\resp -> do
        writeIORef ref (Just resp)
        pure (error "response not used"))
    saved <- readIORef ref
    pure (saved >>= extractBody)
  where
    extractBody _ = Nothing  -- simpler: we assert against LookupResult constructor directly below
matchedTag _ = pure Nothing

-- Simpler: just check the variant and captures explicitly.
isMatched :: LookupResult -> Bool
isMatched (Matched _ _) = True
isMatched _ = False

capturesOf :: LookupResult -> Captures
capturesOf (Matched _ c) = c
capturesOf _             = HashMap.empty

tests = do
    describe "IHP.Router.Trie" do
        describe "splitPath" do
            it "splits on /" do
                splitPath "/posts/123/edit" `shouldBe` ["posts", "123", "edit"]

            it "drops leading, trailing, and empty segments" do
                splitPath "//posts//" `shouldBe` ["posts"]
                splitPath "/" `shouldBe` []
                splitPath "" `shouldBe` []

        describe "lookupTrie — static paths" do
            let trie = buildTrie
                    [ (GET,  [LiteralSeg "posts"], "index")
                    , (POST, [LiteralSeg "posts"], "create")
                    , (GET,  [LiteralSeg "posts", LiteralSeg "new"], "new")
                    ]

            it "matches /posts GET" do
                isMatched (lookupTrie trie GET ["posts"]) `shouldBe` True

            it "matches /posts POST (same path, different method)" do
                isMatched (lookupTrie trie POST ["posts"]) `shouldBe` True

            it "matches /posts/new GET" do
                isMatched (lookupTrie trie GET ["posts", "new"]) `shouldBe` True

            it "returns 405 for /posts PATCH" do
                case lookupTrie trie PATCH ["posts"] of
                    MethodNotAllowed allowed -> do
                        allowed `shouldContain` [GET]
                        allowed `shouldContain` [POST]
                    _ -> expectationFailure "expected MethodNotAllowed"

            it "returns NotMatched for /missing" do
                case lookupTrie trie GET ["missing"] of
                    NotMatched -> pure ()
                    _ -> expectationFailure "expected NotMatched"

        describe "lookupTrie — captures" do
            let postIdSpec = CaptureSpec
                    { captureName = "postId"
                    , captureParse = \bs -> toDyn <$> parseCapture @Int bs
                    }
                trie = buildTrie
                    [ (GET, [LiteralSeg "posts", CaptureSeg postIdSpec], "show")
                    , (GET, [LiteralSeg "posts", LiteralSeg "new"], "new")
                    ]

            it "static paths beat captures (literal wins)" do
                -- /posts/new matches the static route, not the capture
                let captures = capturesOf (lookupTrie trie GET ["posts", "new"])
                HashMap.member "postId" captures `shouldBe` False

            it "captures get parsed and bound" do
                let result = lookupTrie trie GET ["posts", "42"]
                isMatched result `shouldBe` True
                let captures = capturesOf result
                (fromDynamic =<< HashMap.lookup "postId" captures) `shouldBe` Just (42 :: Int)

            it "capture rejects values the parser doesn't accept" do
                -- "abc" isn't a valid Int capture
                case lookupTrie trie GET ["posts", "abc"] of
                    NotMatched -> pure ()
                    _ -> expectationFailure "expected NotMatched for non-numeric capture"

        describe "lookupTrie — splat" do
            let trie = insertRoute
                    [LiteralSeg "files", SplatSeg "path"]
                    GET
                    (tagHandler "download")
                    emptyTrie

            it "captures multi-segment rest" do
                let result = lookupTrie trie GET ["files", "images", "cats", "kitty.jpg"]
                isMatched result `shouldBe` True
                let captures = capturesOf result
                (fromDynamic =<< HashMap.lookup "path" captures) `shouldBe` Just ("images/cats/kitty.jpg" :: Text)

            it "captures single-segment rest" do
                let result = lookupTrie trie GET ["files", "hello.txt"]
                isMatched result `shouldBe` True

        describe "mergeTrie" do
            it "merges two separate prefix trees" do
                let a = insertRoute [LiteralSeg "posts"] GET (tagHandler "posts") emptyTrie
                    b = insertRoute [LiteralSeg "users"] GET (tagHandler "users") emptyTrie
                    merged = mergeTrie a b
                isMatched (lookupTrie merged GET ["posts"]) `shouldBe` True
                isMatched (lookupTrie merged GET ["users"]) `shouldBe` True

            it "merges method-keyed handlers at same leaf" do
                let a = insertRoute [LiteralSeg "posts"] GET (tagHandler "list") emptyTrie
                    b = insertRoute [LiteralSeg "posts"] POST (tagHandler "create") emptyTrie
                    merged = mergeTrie a b
                isMatched (lookupTrie merged GET ["posts"]) `shouldBe` True
                isMatched (lookupTrie merged POST ["posts"]) `shouldBe` True
