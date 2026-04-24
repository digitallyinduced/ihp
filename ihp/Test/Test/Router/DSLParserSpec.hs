{-|
Module: Test.Router.DSLParserSpec
Copyright: (c) digitally induced GmbH, 2026
-}
module Test.Router.DSLParserSpec where

import Test.Hspec
import IHP.Prelude
import IHP.Router.DSL.AST
import IHP.Router.DSL.Parser
import qualified Data.Text as Text
import Network.HTTP.Types.Method (StdMethod (..))

-- | Shorthand for building expected Routes values.
mk :: Text -> [Route] -> Routes
mk name rs = Routes { controllerName = Just name, routes = rs }

rt :: Int -> [Method] -> [PathSeg] -> Text -> Route
rt line ms ps name = Route ms ps (ActionRef name []) line

rtWithBinds :: Int -> [Method] -> [PathSeg] -> Text -> [(Text, Text)] -> Route
rtWithBinds line ms ps name bs = Route ms ps (ActionRef name bs) line

tests = do
    describe "IHP.Router.DSL.Parser" do
        describe "happy path" do
            it "parses a header-only block" do
                parseRoutes "PostsController\n"
                    `shouldBe` Right (mk "PostsController" [])

            it "parses a single static route" do
                parseRoutes "PostsController\nGET /posts PostsAction\n"
                    `shouldBe` Right (mk "PostsController"
                        [ rt 2 [GET] [Literal "posts"] "PostsAction" ])

            it "parses multiple routes" do
                let source = Text.unlines
                        [ "PostsController"
                        , "GET    /posts          PostsAction"
                        , "POST   /posts          CreatePostAction"
                        , "GET    /posts/new      NewPostAction"
                        ]
                parseRoutes source
                    `shouldBe` Right (mk "PostsController"
                        [ rt 2 [GET]  [Literal "posts"] "PostsAction"
                        , rt 3 [POST] [Literal "posts"] "CreatePostAction"
                        , rt 4 [GET]  [Literal "posts", Literal "new"] "NewPostAction"
                        ])

            it "parses captures (RFC 6570 {name})" do
                parseRoutes "PostsController\nGET /posts/{postId} ShowPostAction\n"
                    `shouldBe` Right (mk "PostsController"
                        [ rt 2 [GET] [Literal "posts", Capture "postId" Nothing] "ShowPostAction" ])

            it "parses captures with type annotations" do
                parseRoutes "SearchController\nGET /search/{q:Text} SearchAction\n"
                    `shouldBe` Right (mk "SearchController"
                        [ rt 2 [GET] [Literal "search", Capture "q" (Just "Text")] "SearchAction" ])

            it "parses splat (RFC 6570 {+name})" do
                parseRoutes "FilesController\nGET /files/{+path} DownloadAction\n"
                    `shouldBe` Right (mk "FilesController"
                        [ rt 2 [GET] [Literal "files", Splat "path" Nothing] "DownloadAction" ])

            it "parses multiple methods on one line" do
                parseRoutes "PostsController\nGET|HEAD /posts/{postId} ShowPostAction\n"
                    `shouldBe` Right (mk "PostsController"
                        [ rt 2 [GET, HEAD] [Literal "posts", Capture "postId" Nothing] "ShowPostAction" ])

            it "expands ANY to all StdMethod constructors" do
                case parseRoutes "WebhookController\nANY /webhook WebhookAction\n" of
                    Right Routes { routes = [Route { routeMethods }] } -> do
                        -- http-types' StdMethod has 9 members:
                        -- GET POST HEAD PUT DELETE TRACE CONNECT OPTIONS PATCH
                        length routeMethods `shouldBe` 9
                        routeMethods `shouldContain` [GET]
                        routeMethods `shouldContain` [POST]
                        routeMethods `shouldContain` [PATCH]
                    _ -> expectationFailure "expected a single route with expanded methods"

            it "parses explicit field bindings" do
                parseRoutes "MemberController\nGET /orgs/{org}/users/{user} ShowMemberAction { organizationId = #org, userId = #user }\n"
                    `shouldBe` Right (mk "MemberController"
                        [ rtWithBinds 2 [GET]
                            [ Literal "orgs"
                            , Capture "org" Nothing
                            , Literal "users"
                            , Capture "user" Nothing
                            ]
                            "ShowMemberAction"
                            [("organizationId", "org"), ("userId", "user")]
                        ])

            it "ignores blank lines and comments" do
                let source = Text.unlines
                        [ "-- routes block starts below"
                        , "PostsController"
                        , ""
                        , "GET /posts PostsAction -- index"
                        , "-- create"
                        , "POST /posts CreatePostAction"
                        ]
                case parseRoutes source of
                    Right Routes { controllerName = "PostsController", routes = rs } ->
                        length rs `shouldBe` 2
                    _ -> expectationFailure "expected two routes parsed, comments ignored"

            it "parses /" do
                parseRoutes "HomeController\nGET / HomeAction\n"
                    `shouldBe` Right (mk "HomeController"
                        [ rt 2 [GET] [] "HomeAction" ])


        describe "errors" do
            it "accepts empty block (header-less, zero routes)" do
                parseRoutes "" `shouldBe`
                    Right Routes { controllerName = Nothing, routes = [] }

            it "treats a single non-identifier line as a malformed route" do
                -- When the first line isn't an uppercase identifier, the parser
                -- assumes it's a route line. "123Foo" then fails as an unknown
                -- HTTP method.
                case parseRoutes "123Foo\n" of
                    Left e -> (cs (errorMessage e) :: String) `shouldContain` "unknown method"
                    Right _ -> expectationFailure "expected ParseError"

            it "rejects unknown method" do
                case parseRoutes "C\nYEET /foo FooAction\n" of
                    Left e -> do
                        errorLine e `shouldBe` 2
                        (cs (errorMessage e) :: String) `shouldContain` "unknown method"
                    Right _ -> expectationFailure "expected ParseError"

            it "rejects path not starting with /" do
                case parseRoutes "C\nGET posts PostsAction\n" of
                    Left e -> (cs (errorMessage e) :: String) `shouldContain` "must start with"
                    Right _ -> expectationFailure "expected ParseError"

            it "rejects invalid capture name" do
                case parseRoutes "C\nGET /posts/{123} ShowAction\n" of
                    Left e -> (cs (errorMessage e) :: String) `shouldContain` "invalid capture name"
                    Right _ -> expectationFailure "expected ParseError"

            it "rejects unterminated capture" do
                case parseRoutes "C\nGET /posts/{postId ShowAction\n" of
                    Left e -> (cs (errorMessage e) :: String) `shouldContain` "missing closing '}'"
                    Right _ -> expectationFailure "expected ParseError"

            it "rejects missing action" do
                case parseRoutes "C\nGET /posts\n" of
                    Left _  -> pure ()
                    Right _ -> expectationFailure "expected ParseError"
