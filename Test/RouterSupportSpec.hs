{-|
Module: Test.RouterSupportSpec

Tests for typed auto routing.
-}
module Test.RouterSupportSpec where
import ClassyPrelude
import           Test.Hspec
import           IHP.Test.Mocking
import           IHP.Prelude
import           IHP.QueryBuilder
import           IHP.Environment
import           IHP.FrameworkConfig
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


instance AutoRoute TestController

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
    get #simpleStatus response `shouldBe` status404

tests :: Spec
tests = beforeAll (option Development |> mockContextNoDatabase WebApplication) do
    describe "Typed Auto Route" $ do
        it "parses empty route" $ withContext do
            runSession (testGet "test/Test") Server.application >>= assertSuccess "TestAction"
        it "parses Text param" $ withContext do
            runSession (testGet "test/TestText?firstParam=hello") Server.application >>= assertSuccess "hello"
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





