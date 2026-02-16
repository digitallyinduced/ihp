{-|
Module: Test.RouterSupportSpec

Tests for typed auto routing.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.RouterSupportSpec where
import ClassyPrelude
import Test.Hspec
import IHP.Test.Mocking
import IHP.Prelude
import IHP.Environment
import IHP.FrameworkConfig
import IHP.RouterSupport hiding (get)
import Data.Attoparsec.ByteString.Char8 (string, endOfInput)
import IHP.ViewPrelude
import IHP.ControllerPrelude hiding (get, request)
import Network.Wai.Test
import Network.HTTP.Types

data Band' = Band {id :: (Id' "bands"), meta :: MetaBag} deriving (Eq, Show)
type Band = Band'
type instance GetTableName (Band') = "bands"
type instance GetModelByTableName "bands" = Band
type instance GetModelName (Band') = "Band"
type instance PrimaryKey "bands" = Integer

data Performance' = Performance {id :: (Id' "performances"), meta :: MetaBag} deriving (Eq, Show)
type Performance = Performance'
type instance GetTableName (Performance') = "performances"
type instance GetModelByTableName "performances" = Performance
type instance GetModelName (Performance') = "Performance"
type instance PrimaryKey "performances" = UUID

data TextModel' = TextModel {id :: (Id' "textModel"), meta :: MetaBag} deriving (Eq, Show)
type TextModel = TextModel'
type instance GetTableName (TextModel') = "textModel"
type instance GetModelByTableName "textModel" = TextModel
type instance GetModelName (TextModel') = "textModel"
type instance PrimaryKey "textModel" = Text

data WebApplication = WebApplication deriving (Eq, Show, Data)

data TestController
  = TestAction
  | TestTextAction { firstParam :: Text }
  | TestMaybeTextAction { maybeFirstParam :: Maybe Text }
  | TestIntAction { intParam :: Int }
  | TestMaybeIntAction { maybeInt :: Maybe Int }
  | TestTextListAction { textList :: [Text] }
  | TestIntListAction { intList :: [Int] }
  | TestMixedAction { text :: Text, textOther :: Text, intList :: [Int], maybeText :: Maybe Text, textOtherOther :: Text, intParam :: Int }
  | TestInteger { p1 :: Integer, p2 :: Maybe Integer, p3 :: [Integer] }
  | TestIntegerId { integerId :: Id Band }
  | TestUUIDId { uuidId :: Id Performance }
  | TestUUIDList { uuidList :: [UUID] }
  deriving (Eq, Show, Data)

instance Controller TestController where
    action TestAction = do
        renderPlain "TestAction"
    action TestTextAction { .. } = do
        renderPlain (cs firstParam)
    action TestMaybeTextAction { .. } = do
        case maybeFirstParam of
            Just firstParam -> renderPlain ("Just " <> cs firstParam)
            Nothing -> renderPlain "Nothing"
    action TestIntAction { .. } = do
        renderPlain (cs $ ClassyPrelude.show intParam)
    action TestMaybeIntAction { .. } = do
        case maybeInt of
            Just int -> renderPlain ("Just " <> cs (ClassyPrelude.show int))
            Nothing -> renderPlain "Nothing"
    action TestTextListAction { .. } = do
        renderPlain (cs $ ClassyPrelude.show textList)
    action TestIntListAction { .. } = do
        renderPlain (cs $ ClassyPrelude.show intList)
    action TestMixedAction { .. } = do
        renderPlain (cs $
            text
            <> " " <> textOther
            <> " " <> cs (ClassyPrelude.show intList)
            <> " Nothing"
            <> " " <> textOtherOther
            <> " " <> cs (ClassyPrelude.show intParam))
    action TestInteger { .. } = do
        renderPlain (cs $
            cs (ClassyPrelude.show p1)
            <> (" " :: Text) <> cs (ClassyPrelude.show p2)
            <> " " <> cs (ClassyPrelude.show p3))
    action TestIntegerId { .. } = do
        renderPlain (cs $ ClassyPrelude.show integerId)
    action TestUUIDId { .. } = do
        renderPlain (cs $ ClassyPrelude.show uuidId)
    action TestUUIDList { .. } = do
        renderPlain $ cs $ ClassyPrelude.show uuidList

instance AutoRoute TestController where
    autoRoute = autoRouteWithIdType (parseIntegerId @(Id Band))
    applyAction = applyConstr (parseIntegerId @(Id Band))

-- | Controller for testing customRoutes/customPathTo
data CustomRouteController
  = ListPerformancesAction
  | ShowPerformanceAction { performanceId :: !(Id Performance) }
  | CreatePerformanceAction
  deriving (Eq, Show, Data)

instance Controller CustomRouteController where
    action ListPerformancesAction = renderPlain "ListPerformancesAction"
    action ShowPerformanceAction { .. } = renderPlain (cs $ ClassyPrelude.show performanceId)
    action CreatePerformanceAction = renderPlain "CreatePerformanceAction"

instance AutoRoute CustomRouteController where
    customRoutes = do
        string "/performances/"
        performanceId <- parseId
        endOfInput
        onlyAllowMethods [GET, HEAD]
        pure ShowPerformanceAction { performanceId }

    customPathTo ShowPerformanceAction { performanceId } = Just ("/performances/" <> IHP.Prelude.tshow performanceId)
    customPathTo _ = Nothing

instance FrontController WebApplication where
  controllers =
    [ parseRoute @TestController
    , parseRoute @CustomRouteController
    ]

defaultLayout :: Html -> Html
defaultLayout inner =  [hsx|{inner}|]

instance InitControllerContext WebApplication where
  initContext = do
    setLayout defaultLayout

instance FrontController RootApplication where
    controllers = [ mountFrontController WebApplication ]

testGet :: ByteString -> Session SResponse
testGet url = request $ setPath defaultRequest { requestMethod = methodGet } url

assertSuccess :: ByteString -> SResponse -> IO ()
assertSuccess body response = do
    response.simpleStatus `shouldBe` status200
    response.simpleBody `shouldBe` (cs body)

assertFailure :: SResponse -> IO ()
assertFailure response = do
    response.simpleStatus `shouldBe` status400

config = do
    option Development
    option (AppPort 8000)

tests :: Spec
tests = aroundAll (withMockContextAndApp WebApplication config) do
    describe "Typed Auto Route" $ do
        it "parses empty route" $ withContextAndApp \application -> do
            runSession (testGet "test/Test") application >>= assertSuccess "TestAction"
        it "parses Text param" $ withContextAndApp \application -> do
            runSession (testGet "test/TestText?firstParam=hello") application >>= assertSuccess "hello"
        it "parses Text param with UUID value" $ withContextAndApp \application -> do
                runSession (testGet "test/TestText?firstParam=ea9cd792-107f-49ff-92a1-f610f7a31f31") application >>= assertSuccess "ea9cd792-107f-49ff-92a1-f610f7a31f31"
        it "parses Maybe Text param: Nothing" $ withContextAndApp \application -> do
            runSession (testGet "test/TestMaybeText") application >>= assertSuccess "Nothing"
        it "parses Maybe Text param: Just" $ withContextAndApp \application -> do
            runSession (testGet "test/TestMaybeText?maybeFirstParam=asdfasdf") application >>= assertSuccess "Just asdfasdf"
        it "parses Int param" $ withContextAndApp \application -> do
            runSession (testGet "test/TestInt?intParam=5432") application >>= assertSuccess "5432"
        it "parses Int param: fails on wrong type" $ withContextAndApp \application -> do
            runSession (testGet "test/TestInt?intParam=hello") application >>= assertFailure
        it "parses Maybe Int param: Nothing" $ withContextAndApp \application -> do
            runSession (testGet "test/TestMaybeInt") application >>= assertSuccess "Nothing"
        it "parses Maybe Int param: Just" $ withContextAndApp \application -> do
            runSession (testGet "test/TestMaybeInt?maybeInt=5") application >>= assertSuccess "Just 5"
        it "parses Maybe Int param: Just, wrong type" $ withContextAndApp \application -> do
            runSession (testGet "test/TestMaybeInt?maybeInt=asdf") application >>= assertSuccess "Nothing"
        it "parses [Text] param: empty" $ withContextAndApp \application -> do
            runSession (testGet "test/TestTextList") application >>= assertSuccess "[]"
        it "parses [Text] param: one element" $ withContextAndApp \application -> do
            runSession (testGet "test/TestTextList?textList=hello") application >>= assertSuccess "[\"hello\"]"
        it "parses [Text] param: multiple elements" $ withContextAndApp \application -> do
            runSession (testGet "test/TestTextList?textList=hello,sailor,beautiful,day,5") application >>= assertSuccess "[\"hello\",\"sailor\",\"beautiful\",\"day\",\"5\"]"
        it "parses [Int] param: empty" $ withContextAndApp \application -> do
            runSession (testGet "test/TestIntList") application >>= assertSuccess "[]"
        it "parses [Int] param: one element" $ withContextAndApp \application -> do
            runSession (testGet "test/TestIntList?intList=5") application >>= assertSuccess "[5]"
        it "parses [Int] param: multiple elements" $ withContextAndApp \application -> do
            runSession (testGet "test/TestIntList?intList=5,4,3") application >>= assertSuccess "[5,4,3]"
        it "parses [Int] param: ignore non-int element" $ withContextAndApp \application -> do
            runSession (testGet "test/TestIntList?intList=5,BOO,3") application >>= assertSuccess "[5,3]"
        it "parses mixed params" $ withContextAndApp \application -> do
            runSession (testGet "test/TestMixed?text=hello&textOther=sailor&intList=5,BOO,3&textOtherOther=asdf&intParam=123") application >>= assertSuccess "hello sailor [5,3] Nothing asdf 123"
        it "parses Integer params: empty" $ withContextAndApp \application -> do
            runSession (testGet "test/TestInteger?p1=1237124971624971247691279641762412786418697247869124") application >>= assertSuccess "1237124971624971247691279641762412786418697247869124 Nothing []"
        it "parses Integer params: full" $ withContextAndApp \application -> do
            runSession (testGet "test/TestInteger?p1=1237124971624971247691279641762412786418697247869124&p2=123123197269176247612461769284769812481278487124&p3=1,2,3,4") application >>= assertSuccess "1237124971624971247691279641762412786418697247869124 Just 123123197269176247612461769284769812481278487124 [1,2,3,4]"
        it "parses Id with Integer param" $ withContextAndApp \application -> do
            runSession (testGet "test/TestIntegerId?integerId=123") application >>= assertSuccess "123"
        it "parses Id with UUID param" $ withContextAndApp \application -> do
            runSession (testGet "test/TestUUIDId?uuidId=8dd57d19-490a-4323-8b94-6081ab93bf34") application >>= assertSuccess "8dd57d19-490a-4323-8b94-6081ab93bf34"
        it "parses [UUID] param: empty" $ withContextAndApp \application -> do
            runSession (testGet "test/TestUUIDList") application >>= assertSuccess "[]"
        it "parses [UUID] param: one element" $ withContextAndApp \application -> do
            runSession (testGet "test/TestUUIDList?uuidList=8dd57d19-490a-4323-8b94-6081ab93bf34") application >>= assertSuccess "[8dd57d19-490a-4323-8b94-6081ab93bf34]"
        it "parses [UUID] param: multiple elements" $ withContextAndApp \application -> do
            runSession (testGet "test/TestUUIDList?uuidList=8dd57d19-490a-4323-8b94-6081ab93bf34,8dd57d19-490a-4323-8b94-6081ab93bf34") application >>= assertSuccess "[8dd57d19-490a-4323-8b94-6081ab93bf34,8dd57d19-490a-4323-8b94-6081ab93bf34]"
        it "parses [UUID] param: multiple elements, ignoring non UUID" $ withContextAndApp \application -> do
            runSession (testGet "test/TestUUIDList?uuidList=8dd57d19-490a-4323-8b94-6081ab93bf34,423423432432432") application >>= assertSuccess "[8dd57d19-490a-4323-8b94-6081ab93bf34]"
    describe "pathTo" $ do
        it "generates correct path for empty route" $ withContextAndApp \application -> do
            pathTo TestAction `shouldBe` "/test/Test"
        it "generates correct path for Text param" $ withContextAndApp \application -> do
            pathTo (TestTextAction "hello") `shouldBe` "/test/TestText?firstParam=hello"
        it "generates correct path for Maybe Text param: Nothing" $ withContextAndApp \application -> do
            pathTo (TestMaybeTextAction Nothing) `shouldBe` "/test/TestMaybeText"
        it "generates correct path for Maybe Text param: Just" $ withContextAndApp \application -> do
            pathTo (TestMaybeTextAction (Just "hello")) `shouldBe` "/test/TestMaybeText?maybeFirstParam=hello"
        it "generates correct path for Int param" $ withContextAndApp \application -> do
            pathTo (TestIntAction 5) `shouldBe` "/test/TestInt?intParam=5"
        it "generates correct path for [Text] param: Empty" $ withContextAndApp \application -> do
            pathTo (TestTextListAction []) `shouldBe` "/test/TestTextList"
        it "generates correct path for [Text] param: Full" $ withContextAndApp \application -> do
            pathTo (TestTextListAction ["hello", "there"]) `shouldBe` "/test/TestTextList?textList=hello%2Cthere"
        it "generates correct path for [Int] param" $ withContextAndApp \application -> do
            pathTo (TestIntListAction [1,2,3]) `shouldBe` "/test/TestIntList?intList=1%2C2%2C3"
        it "generates correct path for UUID param" $ withContextAndApp \application -> do
            pathTo (TestUUIDId "8dd57d19-490a-4323-8b94-6081ab93bf34") `shouldBe` "/test/TestUUIDId?uuidId=8dd57d19-490a-4323-8b94-6081ab93bf34"
        it "generates correct path for [UUID] param" $ withContextAndApp \application -> do
            pathTo (TestUUIDList ["8dd57d19-490a-4323-8b94-6081ab93bf34", "fdb15f8e-2fe9-441a-ae0e-da56956b1722"]) `shouldBe` "/test/TestUUIDList?uuidList=8dd57d19-490a-4323-8b94-6081ab93bf34%2Cfdb15f8e-2fe9-441a-ae0e-da56956b1722"
        it "generates correct path when used with Breadcrumbs" $ withContextAndApp \application -> do
            let breadcrumb = breadcrumbLink "Test" TestAction
            breadcrumb.url `shouldBe` Just "/test/Test"
    describe "customRoutes" $ do
        it "parses custom route for overridden action" $ withContextAndApp \application -> do
            runSession (testGet "performances/8dd57d19-490a-4323-8b94-6081ab93bf34") application >>= assertSuccess "8dd57d19-490a-4323-8b94-6081ab93bf34"
        it "auto-generated route still works for overridden action" $ withContextAndApp \application -> do
            runSession (testGet "test/ShowPerformance?performanceId=8dd57d19-490a-4323-8b94-6081ab93bf34") application >>= assertSuccess "8dd57d19-490a-4323-8b94-6081ab93bf34"
        it "auto-generated route works for non-overridden actions" $ withContextAndApp \application -> do
            runSession (testGet "test/ListPerformances") application >>= assertSuccess "ListPerformancesAction"
        it "auto-generated POST route works for non-overridden actions" $ withContextAndApp \application -> do
            let postReq url = request $ setPath defaultRequest { requestMethod = methodPost } url
            runSession (postReq "test/CreatePerformance") application >>= assertSuccess "CreatePerformanceAction"
    describe "customPathTo" $ do
        it "generates custom path for overridden action" $ withContextAndApp \application -> do
            pathTo (ShowPerformanceAction "8dd57d19-490a-4323-8b94-6081ab93bf34") `shouldBe` "/performances/8dd57d19-490a-4323-8b94-6081ab93bf34"
        it "generates auto path for non-overridden action" $ withContextAndApp \application -> do
            pathTo ListPerformancesAction `shouldBe` "/test/ListPerformances"
