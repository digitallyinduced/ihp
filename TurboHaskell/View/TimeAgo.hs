{-|
Module: TurboHaskell.View.TimeAgo
Description: View Helpers for dealing with Time and Dates
Copyright: (c) digitally induced GmbH, 2020
-}
module TurboHaskell.View.TimeAgo (timeAgo, dateTime) where

import TurboHaskell.Prelude
import Data.Time.Format.ISO8601 (iso8601Show)
import TurboHaskell.View.ConvertibleStrings ()
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- | __Display time like @5 minutes ago@__
--
-- Render's a @\<time\>@ HTML-Element. Will be displayed like @5 minutes ago@, @1 day ago@, etc..
-- 
-- Requires the javascript helpers to be available. Then the time will displayed in the current browser
-- timezone.
--
-- When the js helper is not available, a raw ISO8601 time will be shown instead.
--
-- __Example:__ Generated HTML
--
-- >>> <div>{timeAgo (get #createdAt project)}</div>
-- <div><time class="time-ago">2007-08-31T16:47+00:00</div>
--
-- __Example:__ HTML after javascript helpers have been applied
--
-- >>> <div>{timeAgo (get #createdAt project)}</div>
-- <div><time class="time-ago">a while ago</div>
timeAgo :: UTCTime -> Html
timeAgo = timeElement "time-ago"

-- | __Display time like @31.08.2007, 16:47 Uhr@__
--
-- Render's a @\<time\>@ HTML-Element for displaying time and date.
-- 
-- Requires the javascript helpers to be available. Then the date and time will displayed in the current browser
-- timezone.
--
-- The js helper uses `toLocaleDateString` to display the date in the browsers locale format.
--
-- __Example:__ Generated HTML
--
-- >>> <div>{dateTime (get #createdAt project)}</div>
-- <div><time class="date-time">2007-08-31T16:47+00:00</div>
--
-- __Example:__ HTML after javascript helpers have been applied
--
-- >>> <div>{dateTime (get #createdAt project)}</div>
-- <div><time class="date-time">31.08.2007, 16:47 Uhr</div>
dateTime :: UTCTime -> Html
dateTime = timeElement "date-time"

timeElement :: Text -> UTCTime-> Html
timeElement className dateTime = H.time ! A.class_ (cs className) ! A.datetime (cs $ iso8601Show dateTime) $ cs (iso8601Show dateTime)