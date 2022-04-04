module IHP.SEO.Sitemap.Types
( Sitemap(..)
, SitemapLink(..)
, SitemapChangeFrequency(..)
)
where

import IHP.Prelude
import Prelude (Show(..))

data Sitemap
    = Sitemap { links :: [SitemapLink] }
    deriving (Eq, Show, Data)

data SitemapLink
    = SitemapLink { url :: Text, lastModified :: Maybe UTCTime, changeFrequency :: Maybe SitemapChangeFrequency }
    deriving (Eq, Show, Data)

data SitemapChangeFrequency
    = Always
    | Hourly
    | Daily
    | Weekly
    | Monthly
    | Yearly
    | Never
    deriving (Eq, Data)

instance Show SitemapChangeFrequency where
    show Always = "always"
    show Hourly = "hourly"
    show Daily = "daily"
    show Weekly = "weekly"
    show Monthly = "monthly"
    show Yearly = "yearly"
    show Never = "never"
