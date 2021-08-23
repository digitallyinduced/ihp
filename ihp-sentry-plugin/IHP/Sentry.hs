module IHP.Sentry where

import IHP.Prelude
import IHP.FrameworkConfig
import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp

import qualified System.Log.Raven as Raven
import qualified System.Log.Raven.Transport.HttpConduit as Raven
import qualified System.Log.Raven.Types as Raven

-- Sets up sentry as the exception tracker used by IHP. Use this inside your Config.hs
--
-- __Example:__ Usage inside @Config/Config.hs@
--
-- > import IHP.Sentry.ExceptionTracker
-- > 
-- > config :: ConfigBuilder
-- > config = do
-- >     -- ...
-- >     initSentry "YOUR-SENTRY-DSN"
--
initSentry :: String -> ConfigBuilder
initSentry dsn = do
    raven <- liftIO $ Raven.initRaven dsn id Raven.sendRecord Raven.silentFallback
    option ExceptionTracker { onException = sentryOnException raven }

sentryOnException :: Raven.SentryService -> Maybe Request -> SomeException -> IO ()
sentryOnException raven request exception = when (Warp.defaultShouldDisplayException exception) do
        Raven.register raven "ihp" Raven.Error message (recordUpdate request)
        Warp.defaultOnException request exception
    where
        message :: String
        message = displayException exception

        recordUpdate :: Maybe Request -> Raven.SentryRecord -> Raven.SentryRecord
        recordUpdate Nothing record = record
        recordUpdate (Just request) record = record
            { Raven.srCulprit = Just $ cs $ rawPathInfo request
            , Raven.srServerName = cs <$> requestHeaderHost request
            }