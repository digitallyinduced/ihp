{-|
Module: Test.Router.DSLParserSpec
Copyright: (c) digitally induced GmbH, 2026
-}
module Test.Router.DSLParserSpec where

import Test.Hspec
import IHP.Prelude
import "ihp" IHP.Router.DSL.AST
import "ihp" IHP.Router.DSL.Parser
import qualified Data.Text as Text
import Network.HTTP.Types.Method (StdMethod (..))

-- | Shorthand for building expected Routes values.
mk :: Text -> [Route] -> Routes
mk name rs = Routes { controllerName = Just name, routes = rs }

rt :: Int -> [Method] -> [PathSeg] -> Text -> Route
rt line ms ps name = Route ms ps [] (ActionRef name []) line HttpRoute []

rtQ :: Int -> [Method] -> [PathSeg] -> [Text] -> Text -> Route
rtQ line ms ps qs name = Route ms ps qs (ActionRef name []) line HttpRoute []

rtWithBinds :: Int -> [Method] -> [PathSeg] -> Text -> [(Text, Text)] -> Route
rtWithBinds line ms ps name bs = Route ms ps [] (ActionRef name bs) line HttpRoute []

rtWS :: Int -> [PathSeg] -> Text -> Route
rtWS line ps name = Route [GET] ps [] (ActionRef name []) line WebSocketRoute []

ann :: Int -> Text -> Maybe Text -> RouteAnnotation
ann line name value =
    RouteAnnotation
        { annotationName = name
        , annotationValue = value
        , annotationLine = line
        }

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

            it "parses a single query param" do
                parseRoutes "ThreadsController\nGET /ShowThread?threadId ShowThreadAction\n"
                    `shouldBe` Right (mk "ThreadsController"
                        [ rtQ 2 [GET] [Literal "ShowThread"] ["threadId"] "ShowThreadAction" ])

            it "parses multiple query params (& separated)" do
                parseRoutes "SearchController\nGET /search?q&page&tags SearchAction\n"
                    `shouldBe` Right (mk "SearchController"
                        [ rtQ 2 [GET] [Literal "search"] ["q", "page", "tags"] "SearchAction" ])

            it "parses query params after a path with captures" do
                parseRoutes "ThreadsController\nGET /threads/{threadId}?page ShowThreadAction\n"
                    `shouldBe` Right (mk "ThreadsController"
                        [ rtQ 2 [GET]
                            [Literal "threads", Capture "threadId" Nothing]
                            ["page"]
                            "ShowThreadAction"
                        ])

            it "parses a WS route as WebSocketRoute kind" do
                parseRoutes "webRoutes\nWS /chat ChatApp\n"
                    `shouldBe` Right Routes
                        { controllerName = Just "webRoutes"
                        , routes = [ rtWS 2 [Literal "chat"] "ChatApp" ]
                        }

            it "parses WS routes alongside HTTP routes" do
                let source = Text.unlines
                        [ "webRoutes"
                        , "GET /posts        PostsAction"
                        , "WS  /chat         ChatApp"
                        ]
                parseRoutes source
                    `shouldBe` Right Routes
                        { controllerName = Just "webRoutes"
                        , routes =
                            [ rt 2 [GET] [Literal "posts"] "PostsAction"
                            , rtWS 3 [Literal "chat"] "ChatApp"
                            ]
                        }

            it "attaches indented metadata lines to the previous route" do
                let source = Text.unlines
                        [ "ApiController"
                        , "GET /bands/{bandId}?page&tags ShowBandAction { bandTags = #tags }"
                        , "  summary: Show a band payload"
                        , "  tags: Bands, Search"
                        , "POST /sessions CreateSessionAction"
                        , "  success: 201 Created response"
                        ]
                    showRoute =
                        (rtQ 2 [GET] [Literal "bands", Capture "bandId" Nothing] ["page", "tags"] "ShowBandAction")
                            { routeAction = ActionRef "ShowBandAction" [("bandTags", "tags")]
                            , routeAnnotations =
                                [ ann 3 "summary" (Just "Show a band payload")
                                , ann 4 "tags" (Just "Bands, Search")
                                ]
                            }
                    createRoute =
                        (rt 5 [POST] [Literal "sessions"] "CreateSessionAction")
                            { routeAnnotations = [ann 6 "success" (Just "201 Created response")]
                            }
                parseRoutes source
                    `shouldBe` Right (mk "ApiController" [showRoute, createRoute])

            it "parses flag metadata such as private" do
                let source = Text.unlines
                        [ "ApiController"
                        , "GET /raw RawJsonAction"
                        , "  private"
                        ]
                parseRoutes source
                    `shouldBe` Right
                        ( mk
                            "ApiController"
                            [ (rt 2 [GET] [Literal "raw"] "RawJsonAction")
                                { routeAnnotations = [ann 3 "private" Nothing]
                                }
                            ]
                        )

            it "keeps duplicate and unknown metadata syntactically valid" do
                let source = Text.unlines
                        [ "ApiController"
                        , "GET /raw RawJsonAction"
                        , "  summary: One"
                        , "  summary: Two"
                        , "  xCustom: Value"
                        ]
                case parseRoutes source of
                    Right Routes{routes = [Route{routeAnnotations}]} ->
                        routeAnnotations
                            `shouldBe` [ ann 3 "summary" (Just "One")
                                       , ann 4 "summary" (Just "Two")
                                       , ann 5 "xCustom" (Just "Value")
                                       ]
                    _ -> expectationFailure "expected metadata to remain syntactically valid"

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

            it "rejects a route line without an HTTP method" do
                case parseRoutes "C\n/posts/{postId} ShowPostAction\n" of
                    Left e -> do
                        errorLine e `shouldBe` 2
                        (cs (errorMessage e) :: String) `shouldContain` "missing HTTP method"
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

            it "rejects empty query list (trailing '?')" do
                case parseRoutes "C\nGET /items? ShowAction\n" of
                    Left e -> (cs (errorMessage e) :: String) `shouldContain` "empty query-param list"
                    Right _ -> expectationFailure "expected ParseError"

            it "rejects invalid query-param name" do
                case parseRoutes "C\nGET /items?123 ShowAction\n" of
                    Left e -> (cs (errorMessage e) :: String) `shouldContain` "invalid query-param name"
                    Right _ -> expectationFailure "expected ParseError"

            it "rejects empty query-param name (stray &)" do
                case parseRoutes "C\nGET /items?a&&b ShowAction\n" of
                    Left e -> (cs (errorMessage e) :: String) `shouldContain` "empty query-param name"
                    Right _ -> expectationFailure "expected ParseError"

            it "rejects WS combined with HTTP methods" do
                case parseRoutes "webRoutes\nWS|GET /chat ChatApp\n" of
                    Left e -> do
                        errorLine e `shouldBe` 2
                        (cs (errorMessage e) :: String) `shouldContain` "'WS' cannot be combined"
                    Right _ -> expectationFailure "expected ParseError"

            it "rejects WS routes with path captures" do
                case parseRoutes "webRoutes\nWS /chat/{roomId} ChatApp\n" of
                    Left e -> do
                        errorLine e `shouldBe` 2
                        (cs (errorMessage e) :: String) `shouldContain` "WS routes do not support path captures"
                    Right _ -> expectationFailure "expected ParseError"

            it "rejects WS routes with splat captures" do
                case parseRoutes "webRoutes\nWS /chat/{+rest} ChatApp\n" of
                    Left e -> do
                        errorLine e `shouldBe` 2
                        (cs (errorMessage e) :: String) `shouldContain` "WS routes do not support path captures"
                    Right _ -> expectationFailure "expected ParseError"

            it "rejects WS routes with query parameters" do
                case parseRoutes "webRoutes\nWS /chat?room ChatApp\n" of
                    Left e -> do
                        errorLine e `shouldBe` 2
                        (cs (errorMessage e) :: String) `shouldContain` "WS routes do not support query parameters"
                    Right _ -> expectationFailure "expected ParseError"

            it "rejects metadata before any route" do
                case parseRoutes "ApiController\n  summary: Missing route\nGET /raw RawJsonAction\n" of
                    Left e -> do
                        errorLine e `shouldBe` 2
                        (cs (errorMessage e) :: String) `shouldContain` "metadata line must follow a route"
                    Right _ -> expectationFailure "expected ParseError"

            it "rejects invalid metadata names" do
                case parseRoutes "ApiController\nGET /raw RawJsonAction\n  not valid: nope\n" of
                    Left e -> do
                        errorLine e `shouldBe` 3
                        (cs (errorMessage e) :: String) `shouldContain` "invalid metadata name"
                    Right _ -> expectationFailure "expected ParseError"
