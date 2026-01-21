module IHP.OAuth.Google.Config where

import IHP.Prelude
import IHP.FrameworkConfig
import IHP.OAuth.Google.Types
import qualified System.Environment as Env
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.TMap as TMap

-- | The Google client id has to be provided using the @OAUTH_GOOGLE_CLIENT_ID@ env var.
--
-- __Example:__ Configure google oauth in @Config.hs@
--
-- > module Config where
-- > 
-- > import IHP.Prelude
-- > import IHP.Environment
-- > import IHP.FrameworkConfig
-- > import IHP.OAuth.Google.Config
-- > 
-- > config :: ConfigBuilder
-- > config = do
-- >     option Development
-- >     option (AppHostname "localhost")
-- >     initGoogleOAuth
--
initGoogleOAuth :: State.StateT TMap.TMap IO ()
initGoogleOAuth = do
    clientId <- liftIO $ Env.getEnv "OAUTH_GOOGLE_CLIENT_ID"
    option GoogleOAuthConfig { clientId = cs clientId }
