module IHP.SeoSupport.Sitemap.Controller where

import IHP.Prelude
import IHP.RouterPrelude (string, endOfInput, CanRoute(..), HasPath(..))
import IHP.ControllerPrelude
import IHP.SeoSupport.Sitemap.Types
import qualified Data.Map as Map
import qualified Text.XML as XML
import Text.Hamlet.XML

data SitemapController
    = SitemapAction
    deriving (Eq, Show, Data)

instance HasPath SitemapController where
    pathTo SitemapAction = "/sitemap.xml"

instance CanRoute SitemapController where
    parseRoute' = do
        string "/sitemap.xml"
        endOfInput
        pure SitemapAction

renderXmlSitemap :: (?context::ControllerContext) => Sitemap -> IO ()
renderXmlSitemap Sitemap { links } = do
    let xmlDocument = XML.Document (XML.Prologue [] Nothing []) sitemapLinks []
    renderXml $ XML.renderLBS def xmlDocument
    where
        sitemapLinks = XML.Element "urlset" (Map.fromList [("xmlns", "http://www.sitemaps.org/schemas/sitemap/0.9")]) [xml|
            $forall link <- links
                ^{sitemapLink link}
        |]
        sitemapLink SitemapLink { url, lastModified, changeFrequency } = [xml|
            <loc>#{url}
            <lastmod>#{maybe mempty formatUTCTime lastModified}
            <changefreq>#{maybe mempty show changeFrequency}
        |]

formatUTCTime :: UTCTime -> Text
formatUTCTime utcTime = cs $ formatTime defaultTimeLocale "%Y-%m-%d" utcTime
