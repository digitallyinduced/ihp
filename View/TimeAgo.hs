module Foundation.View.TimeAgo where

import           ClassyPrelude                      hiding (UTCTime, (!))
import qualified ClassyPrelude
import           Data.String.Conversions            (cs)
import           Data.Time.Clock                    (UTCTime)
import           Data.Time.Format.ISO8601           (iso8601Show)
import           Foundation.View.ConvertibleStrings ()
import           Text.Blaze.Html5                   (Html)
import           Text.Blaze.Html5                   ((!))
import qualified Text.Blaze.Html5                   as H
import qualified Text.Blaze.Html5.Attributes        as A
import           Unsafe.Coerce

timeAgo :: ClassyPrelude.UTCTime -> Html
timeAgo = timeElement "timeAgo"

dateTime = timeElement "dateTime"

timeElement :: String -> ClassyPrelude.UTCTime-> Html
timeElement className dateTime= H.time ! A.class_ (cs className) ! A.datetime (cs $ formatDateTime dateTime) $ cs (formatDateTime dateTime)
    where formatDateTime time = iso8601Show (unsafeCoerce time :: UTCTime)