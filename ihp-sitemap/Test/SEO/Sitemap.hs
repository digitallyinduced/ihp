module Main where

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
import IHP.Controller.NotFound (handleNotFound)
import IHP.RequestVault

main :: IO ()
main = hspec do
    tests

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
                    { url = urlTo $ ShowPostAction (post.id)
                    , lastModified = Just (post.updatedAt)
                    , changeFrequency = Just Hourly
                    })
        renderXmlSitemap (Sitemap sitemapLinks)

data WebApplication
    = WebApplication
    deriving (Eq, Show, Data)

instance FrontController WebApplication where
  controllers = [ parseRoute @SitemapController ]

instance InitControllerContext WebApplication where
  initContext = pure ()

instance FrontController RootApplication where
    controllers = [ mountFrontController WebApplication ]

instance Worker RootApplication where
    workers _ = []

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

makeApplication = do
    frameworkConfig <- buildFrameworkConfig config
    pure $ frameworkConfigMiddleware frameworkConfig $ Server.application handleNotFound (\app -> app)

tests :: Spec
tests = beforeAll (mockContextNoDatabase WebApplication config) do
    describe "SEO" do
        describe "Sitemap" do
            it "should render a XML Sitemap" $ withContext do
                application <- makeApplication
                runSession (testGet "/sitemap.xml") application
                    >>= assertSuccess "<?xml version=\"1.0\" encoding=\"UTF-8\"?><urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\"><url><loc>http://localhost:8000/main/ShowPost?postId=00000000-0000-0000-0000-000000000000</loc><lastmod>2105-04-16</lastmod><changefreq>hourly</changefreq></url></urlset>"
