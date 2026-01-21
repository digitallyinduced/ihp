module IHP.OAuth.Github.Config where

import IHP.Prelude
import IHP.FrameworkConfig
import IHP.OAuth.Github.Types
import qualified System.Environment as Env
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.TMap as TMap

-- | The Github client id and client secret have to be provided using the @OAUTH_GITHUB_CLIENT_ID@ and @OAUTH_GITHUB_CLIENT_SECRET@ env vars.
--
-- __Example:__ Configure github oauth in @Config.hs@
--
-- > module Config where
-- > 
-- > import IHP.Prelude
-- > import IHP.Environment
-- > import IHP.FrameworkConfig
-- > import IHP.OAuth.Github.Config
-- > 
-- > config :: ConfigBuilder
-- > config = do
-- >     option Development
-- >     option (AppHostname "localhost")
-- >     initGithubOAuth
--
initGithubOAuth :: State.StateT TMap.TMap IO ()
initGithubOAuth = do
    clientId <- liftIO $ Env.getEnv "OAUTH_GITHUB_CLIENT_ID"
    clientSecret <- liftIO $ Env.getEnv "OAUTH_GITHUB_CLIENT_SECRET"
    option GithubOAuthConfig { clientId = cs clientId, clientSecret = cs clientSecret }
