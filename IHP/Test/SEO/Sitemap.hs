module Test.SEO.Sitemap where

import Test.Hspec
import IHP.Test.Mocking
import IHP.Environment
import IHP.ViewPrelude
import IHP.ControllerPrelude hiding (get, request)
import qualified IHP.Server as Server
import Network.Wai
import Network.Wai.Test
import Network.HTTP.Types

import IHP.SEO.Sitemap.Types
import IHP.SEO.Sitemap.Routes
import IHP.SEO.Sitemap.ControllerFunctions

data Post = Post
        { id :: UUID
        , updatedAt :: UTCTime
        }

data PostController
  = ShowPostAction { postId :: UUID }
  deriving (Eq, Show, Data)

instance AutoRoute PostController

instance Controller SitemapController where
    action SitemapAction = do
        let time = UTCTime { utctDay = ModifiedJulianDay (300 * 300), utctDayTime = 300 * 300 }
        let posts = [Post { id = def, updatedAt = time }]
        let sitemapLinks = posts |> map (\post ->
                SitemapLink
                    { url = urlTo $ ShowPostAction (get #id post)
                    , lastModified = Just (get #updatedAt post)
                    , changeFrequency = Just Hourly
                    })
        renderXmlSitemap (Sitemap sitemapLinks)

data WebApplication
    = WebApplication
    deriving (Eq, Show, Data)

instance FrontController WebApplication where
  controllers = [ parseRoute @SitemapController ]

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
    describe "SEO" do
        describe "Sitemap" do
            it "should render a XML Sitemap" $ withContext do
                runSession (testGet "/sitemap.xml") Server.application
                    >>= assertSuccess "<?xml version=\"1.0\" encoding=\"UTF-8\"?><urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\"><url><loc>http://localhost:8000/test/ShowPost?postId=00000000-0000-0000-0000-000000000000</loc><lastmod>2105-04-16</lastmod><changefreq>hourly</changefreq></url></urlset>"
