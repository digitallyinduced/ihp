{-|
Module: IHP.View.TimeAgo
Description: View Helpers for dealing with Time and Dates
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.View.TimeAgo (timeAgo, dateTime, date, time) where

import IHP.Prelude
import Data.Time.Format.ISO8601 (iso8601Show)
import IHP.View.ConvertibleStrings ()
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
-- When the js helper is not available, the time will be displayed with the format: DD.MM.YYYY, HH:MM
--
-- __Example:__ Generated HTML
--
-- >>> <div>{timeAgo (get #createdAt project)}</div>
-- <div><time class="time-ago">31.08.2007, 16:47</time></div>
--
-- __Example:__ HTML after javascript helpers have been applied
--
-- >>> <div>{timeAgo (get #createdAt project)}</div>
-- <div><time class="time-ago">a while ago</time></div>
timeAgo :: UTCTime -> Html
timeAgo = timeElement "time-ago"

-- | __Display time like @31.08.2007, 16:47@__
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
-- <div><time class="date-time">31.08.2007, 16:47</time></div>
--
-- __Example:__ HTML after javascript helpers have been applied
--
-- >>> <div>{dateTime (get #createdAt project)}</div>
-- <div><time class="date-time">31.08.2007, 16:47 Uhr</time></div>
dateTime :: UTCTime -> Html
dateTime = timeElement "date-time"

-- | __Display date like @31.08.2007@__
--
-- Render's a @\<time\>@ HTML-Element for displaying the date.
-- 
-- Requires the javascript helpers to be available. Then the date will displayed in the current browser
-- locale format and timezone.
--
-- The js helper uses `toLocaleDateString` to display the date in the browsers locale format.
--
-- __Example:__ Generated HTML
--
-- >>> <div>{date (get #createdAt project)}</div>
-- <div><time class="date">31.08.2007, 16:47</time></div>
--
-- __Example:__ HTML after javascript helpers have been applied
--
-- >>> <div>{date (get #createdAt project)}</div>
-- <div><time class="date">31.08.2007</time></div>
date :: UTCTime -> Html
date = timeElement "date"

-- | __Display time like @16:47@__
--
-- Render's a @\<time\>@ HTML-Element for displaying the time.
-- 
-- Requires the javascript helpers to be available. Then the time will displayed in the current browser
-- timezone.
--
-- The js helper uses `toLocaleDateString` to display the date in the browsers locale format.
--
-- __Example:__ Generated HTML
--
-- >>> <div>{time (get #createdAt project)}</div>
-- <div><time class="time">16:47</time></div>
--
-- __Example:__ HTML after javascript helpers have been applied
--
-- >>> <div>{time (get #createdAt project)}</div>
-- <div><time class="time">16:47 Uhr</time></div>
time :: UTCTime -> Html
time = timeElement "time"

timeElement :: Text -> UTCTime -> Html
timeElement className dateTime = H.time ! A.class_ (cs className) ! A.datetime (cs $ iso8601Show dateTime) $ cs (beautifyUtcTime dateTime)

beautifyUtcTime :: UTCTime -> String
beautifyUtcTime utcTime = formatTime defaultTimeLocale "%d.%m.%Y, %H:%M" utcTime
