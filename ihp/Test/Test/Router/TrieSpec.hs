{-|
Module: Test.Router.TrieSpec
Copyright: (c) digitally induced GmbH, 2026
-}
module Test.Router.TrieSpec where

import Test.Hspec
import IHP.Prelude
import IHP.Router.Trie
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

isMatched :: LookupResult -> Bool
isMatched (Matched _ _) = True
isMatched _ = False

capturesOf :: LookupResult -> Captures
capturesOf (Matched _ c) = c
capturesOf _             = []

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
            let postIdSpec = CaptureSpec { captureName = "postId" }
                trie = buildTrie
                    [ (GET, [LiteralSeg "posts", CaptureSeg postIdSpec], "show")
                    , (GET, [LiteralSeg "posts", LiteralSeg "new"], "new")
                    ]

            it "static paths beat captures (literal wins)" do
                -- /posts/new matches the static route, so no captures are
                -- collected at all.
                let captures = capturesOf (lookupTrie trie GET ["posts", "new"])
                captures `shouldBe` []

            it "captures get collected as raw ByteStrings in path order" do
                let result = lookupTrie trie GET ["posts", "42"]
                isMatched result `shouldBe` True
                capturesOf result `shouldBe` ["42"]

            it "captures accept any non-empty segment (type validation is the handler's job)" do
                -- With the positional-captures model, the trie doesn't
                -- know or care about capture types. "abc" matches just
                -- as readily as "42"; the handler would decode and 404.
                let result = lookupTrie trie GET ["posts", "abc"]
                isMatched result `shouldBe` True
                capturesOf result `shouldBe` ["abc"]

        describe "lookupTrie — splat" do
            let trie = insertRoute
                    [LiteralSeg "files", SplatSeg "path"]
                    GET
                    (tagHandler "download")
                    emptyTrie

            it "captures multi-segment rest" do
                let result = lookupTrie trie GET ["files", "images", "cats", "kitty.jpg"]
                isMatched result `shouldBe` True
                capturesOf result `shouldBe` ["images/cats/kitty.jpg"]

            it "captures single-segment rest" do
                let result = lookupTrie trie GET ["files", "hello.txt"]
                isMatched result `shouldBe` True
                capturesOf result `shouldBe` ["hello.txt"]

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
