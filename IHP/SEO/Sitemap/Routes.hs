module IHP.SEO.Sitemap.Routes where

import IHP.Prelude
import IHP.RouterPrelude
import IHP.SEO.Sitemap.Types

instance HasPath SitemapController where
    pathTo SitemapAction = "/sitemap.xml"

instance CanRoute SitemapController where
    parseRoute' = do
        string "/sitemap.xml"
        endOfInput
        pure SitemapAction
