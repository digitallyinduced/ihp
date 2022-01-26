{-|
Module: Test.RouterSupportSpec

Tests for typed auto routing.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.RouterSupportSpec where
import qualified Prelude
import ClassyPrelude
import Test.Hspec
import IHP.Test.Mocking
import IHP.Prelude
import IHP.QueryBuilder
import IHP.Environment
import IHP.FrameworkConfig
import IHP.HaskellSupport
import IHP.RouterSupport hiding (get)
import IHP.FrameworkConfig
import IHP.Job.Types
import IHP.Controller.RequestContext hiding (request)
import IHP.ViewPrelude
import IHP.ControllerPrelude hiding (get, request)
import qualified IHP.Server as Server
import Data.Attoparsec.ByteString.Char8 (string, Parser, (<?>), parseOnly, take, endOfInput, choice, takeTill, takeByteString)
import Network.Wai
import Network.Wai.Test
import Network.HTTP.Types
import qualified IHP.ErrorController as ErrorController
import Data.String.Conversions
import Unsafe.Coerce

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

instance AutoRoute TestController where
    autoRoute = autoRouteWithIdType (parseIntegerId @(Id Band))

instance FrontController WebApplication where
  controllers = [ startPage TestAction, parseRoute @TestController ]

defaultLayout :: Html -> Html
defaultLayout inner =  [hsx|{inner}|]

instance InitControllerContext WebApplication where
  initContext = do
    setLayout defaultLayout

instance FrontController RootApplication where
    controllers = [ mountFrontController WebApplication ]

instance Worker RootApplication where
    workers _ = []

testGet :: ByteString -> Session SResponse
testGet url = request $ setPath defaultRequest { requestMethod = methodGet } url

assertSuccess :: ByteString -> SResponse -> IO ()
assertSuccess body response = do
    get #simpleStatus response `shouldBe` status200
    get #simpleBody response `shouldBe` (cs body)

assertFailure :: SResponse -> IO ()
assertFailure response = do
    get #simpleStatus response `shouldBe` status400

config = do
    option Development
    option (AppPort 8000)

tests :: Spec
tests = beforeAll (mockContextNoDatabase WebApplication config) do
    describe "Typed Auto Route" $ do
        it "parses empty route" $ withContext do
            runSession (testGet "test/Test") Server.application >>= assertSuccess "TestAction"
        it "parses Text param" $ withContext do
            runSession (testGet "test/TestText?firstParam=hello") Server.application >>= assertSuccess "hello"
        it "parses Text param with UUID value" $ withContext do
                runSession (testGet "test/TestText?firstParam=ea9cd792-107f-49ff-92a1-f610f7a31f31") Server.application >>= assertSuccess "ea9cd792-107f-49ff-92a1-f610f7a31f31"
        it "parses Maybe Text param: Nothing" $ withContext do
            runSession (testGet "test/TestMaybeText") Server.application >>= assertSuccess "Nothing"
        it "parses Maybe Text param: Just" $ withContext do
            runSession (testGet "test/TestMaybeText?maybeFirstParam=asdfasdf") Server.application >>= assertSuccess "Just asdfasdf"
        it "parses Int param" $ withContext do
            runSession (testGet "test/TestInt?intParam=5432") Server.application >>= assertSuccess "5432"
        it "parses Int param: fails on wrong type" $ withContext do
            runSession (testGet "test/TestInt?intParam=hello") Server.application >>= assertFailure
        it "parses Maybe Int param: Nothing" $ withContext do
            runSession (testGet "test/TestMaybeInt") Server.application >>= assertSuccess "Nothing"
        it "parses Maybe Int param: Just" $ withContext do
            runSession (testGet "test/TestMaybeInt?maybeInt=5") Server.application >>= assertSuccess "Just 5"
        it "parses Maybe Int param: Just, wrong type" $ withContext do
            runSession (testGet "test/TestMaybeInt?maybeInt=asdf") Server.application >>= assertSuccess "Nothing"
        it "parses [Text] param: empty" $ withContext do
            runSession (testGet "test/TestTextList") Server.application >>= assertSuccess "[]"
        it "parses [Text] param: one element" $ withContext do
            runSession (testGet "test/TestTextList?textList=hello") Server.application >>= assertSuccess "[\"hello\"]"
        it "parses [Text] param: multiple elements" $ withContext do
            runSession (testGet "test/TestTextList?textList=hello,sailor,beautiful,day,5") Server.application >>= assertSuccess "[\"hello\",\"sailor\",\"beautiful\",\"day\",\"5\"]"
        it "parses [Int] param: empty" $ withContext do
            runSession (testGet "test/TestIntList") Server.application >>= assertSuccess "[]"
        it "parses [Int] param: one element" $ withContext do
            runSession (testGet "test/TestIntList?intList=5") Server.application >>= assertSuccess "[5]"
        it "parses [Int] param: multiple elements" $ withContext do
            runSession (testGet "test/TestIntList?intList=5,4,3") Server.application >>= assertSuccess "[5,4,3]"
        it "parses [Int] param: ignore non-int element" $ withContext do
            runSession (testGet "test/TestIntList?intList=5,BOO,3") Server.application >>= assertSuccess "[5,3]"
        it "parses mixed params" $ withContext do
            runSession (testGet "test/TestMixed?text=hello&textOther=sailor&intList=5,BOO,3&textOtherOther=asdf&intParam=123") Server.application >>= assertSuccess "hello sailor [5,3] Nothing asdf 123"
        it "parses Integer params: empty" $ withContext do
            runSession (testGet "test/TestInteger?p1=1237124971624971247691279641762412786418697247869124") Server.application >>= assertSuccess "1237124971624971247691279641762412786418697247869124 Nothing []"
        it "parses Integer params: full" $ withContext do
            runSession (testGet "test/TestInteger?p1=1237124971624971247691279641762412786418697247869124&p2=123123197269176247612461769284769812481278487124&p3=1,2,3,4") Server.application >>= assertSuccess "1237124971624971247691279641762412786418697247869124 Just 123123197269176247612461769284769812481278487124 [1,2,3,4]"
        it "parses Id with Integer param" $ withContext do
            runSession (testGet "test/TestIntegerId?integerId=123") Server.application >>= assertSuccess "123"
        it "parses Id with UUID param" $ withContext do
            runSession (testGet "test/TestUUIDId?uuidId=8dd57d19-490a-4323-8b94-6081ab93bf34") Server.application >>= assertSuccess "8dd57d19-490a-4323-8b94-6081ab93bf34"
    describe "pathTo" $ do
        it "generates correct path for empty route" $ withContext do
            pathTo TestAction `shouldBe` "/test/Test"
        it "generates correct path for Text param" $ withContext do
            pathTo (TestTextAction "hello") `shouldBe` "/test/TestText?firstParam=hello"
        it "generates correct path for Maybe Text param: Nothing" $ withContext do
            pathTo (TestMaybeTextAction Nothing) `shouldBe` "/test/TestMaybeText"
        it "generates correct path for Maybe Text param: Just" $ withContext do
            pathTo (TestMaybeTextAction (Just "hello")) `shouldBe` "/test/TestMaybeText?maybeFirstParam=hello"
        it "generates correct path for Int param" $ withContext do
            pathTo (TestIntAction 5) `shouldBe` "/test/TestInt?intParam=5"
        it "generates correct path for [Text] param: Empty" $ withContext do
            pathTo (TestTextListAction []) `shouldBe` "/test/TestTextList"
        it "generates correct path for [Text] param: Full" $ withContext do
            pathTo (TestTextListAction ["hello", "there"]) `shouldBe` "/test/TestTextList?textList=hello%2Cthere"
        it "generates correct path for [Int] param" $ withContext do
            pathTo (TestIntListAction [1,2,3]) `shouldBe` "/test/TestIntList?intList=1%2C2%2C3"
        it "generates correct path when used with Breadcrumbs" $ withContext do
            let breadcrumb = breadcrumbLink "Test" TestAction
            get #url breadcrumb `shouldBe` Just "/test/Test"
