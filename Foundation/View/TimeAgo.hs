module Foundation.View.TimeAgo (timeAgo, dateTime) where

import           ClassyPrelude                      hiding (UTCTime, (!))
import qualified ClassyPrelude
import           Data.String.Conversions            (cs)
import           Data.Time.Clock                    (UTCTime)
import           Data.Time.Format.ISO8601           (iso8601Show)
import           Foundation.View.ConvertibleStrings ()
import           Text.Blaze.Html5                   (Html, (!))
import qualified Text.Blaze.Html5                   as H
import qualified Text.Blaze.Html5.Attributes        as A
import           Unsafe.Coerce

timeAgo :: ClassyPrelude.UTCTime -> Html
timeAgo = timeElement "time-ago"

dateTime = timeElement "date-time"

timeElement :: Text -> ClassyPrelude.UTCTime-> Html
timeElement className dateTime= H.time ! A.class_ (cs className) ! A.datetime (cs $ formatDateTime dateTime) $ cs (formatDateTime dateTime)
    where formatDateTime time = iso8601Show (unsafeCoerce time :: UTCTime)