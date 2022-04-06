module IHP.SEO.Sitemap.Controller where

import IHP.Prelude
import IHP.RouterPrelude (string, endOfInput, CanRoute(..), HasPath(..))
import IHP.ControllerPrelude
import IHP.SEO.Sitemap.Types
import qualified Text.Blaze as Markup
import qualified Text.Blaze.Internal as Markup
import qualified Text.Blaze.Renderer.Utf8 as Markup

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
    let sitemap = Markup.toMarkup [xmlDocument, sitemapLinks]
    renderXml $ Markup.renderMarkup sitemap
    where
        xmlDocument = Markup.preEscapedText "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
        urlSet = Markup.customParent "urlset" Markup.! Markup.customAttribute "xmlns" "http://www.sitemaps.org/schemas/sitemap/0.9"
        sitemapLinks = urlSet (Markup.toMarkup (map sitemapLink links))
        sitemapLink SitemapLink { url, lastModified, changeFrequency } =
            let
                loc = Markup.customParent "loc" (Markup.text url)
                lastMod =  Markup.customParent "lastmod" (Markup.text (maybe mempty formatUTCTime lastModified))
                changeFreq = Markup.customParent "changefreq" (Markup.text (maybe mempty show changeFrequency))
            in
                Markup.toMarkup [loc, lastMod, changeFreq]

formatUTCTime :: UTCTime -> Text
formatUTCTime utcTime = cs (formatTime defaultTimeLocale "%Y-%m-%d" utcTime)
