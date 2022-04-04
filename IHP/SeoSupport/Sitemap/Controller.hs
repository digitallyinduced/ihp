module IHP.SeoSupport.Sitemap.Controller where

import IHP.Prelude
import IHP.RouterPrelude (string, endOfInput, CanRoute(..), HasPath(..))
import IHP.ControllerPrelude
import IHP.SeoSupport.Sitemap.Types

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
    let sitemapStart
            = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
            <> "\n"
            <> "<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">"
    let sitemapEnd = "</urlset>"
    let sitemap = unlines $ [sitemapStart] ++ map renderSitemapLink links ++ [sitemapEnd]
    renderXml (cs sitemap)
    where
        renderSitemapLink SitemapLink { url, lastModified, changeFrequency } =
            let
                loc = "<loc>" <> url <> "</loc>"
                lastMod = lastModified |> maybe mempty (\lM -> "<lastmod>" <> lM <> "</lastmod>")
                changeFreq = changeFrequency |> maybe mempty (\cF -> "<changefreq>" <> show cF <> "</changefreq>")
            in unlines ["<url>", loc, lastMod, changeFreq, "</url>"]
