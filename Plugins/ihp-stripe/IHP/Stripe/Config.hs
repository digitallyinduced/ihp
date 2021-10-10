{-|
Description: Functions to send requests to the stripe API
Copyright: (c) digitally induced GmbH, 2021
-}
module IHP.Stripe.Config where

import IHP.Prelude
import IHP.FrameworkConfig
import IHP.Stripe.Types
import qualified System.Environment as Env
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.TMap as TMap

-- | The stripe public key, secret key and the webhook secret key have to be provided using these env variables:
--
-- 1. @STRIPE_PUBLIC_KEY@
-- 2. @STRIPE_SECRET_KEY@
-- 3. @STRIPE_WEBHOOK_SECRET_KEY@
--
-- __Example:__ Configure stripe in @Config.hs@
--
-- > module Config where
-- > 
-- > import IHP.Prelude
-- > import IHP.Environment
-- > import IHP.FrameworkConfig
-- > import IHP.Stripe.Config
-- > 
-- > config :: ConfigBuilder
-- > config = do
-- >     option Development
-- >     option (AppHostname "localhost")
-- >     initStripe
--
initStripe :: State.StateT TMap.TMap IO ()
initStripe = do
    stripePublicKey <- liftIO $ Env.getEnv "STRIPE_PUBLIC_KEY"
    stripeSecretKey <- liftIO $ Env.getEnv "STRIPE_SECRET_KEY"
    stripeWebhookSecretKey <- liftIO $ Env.getEnv "STRIPE_WEBHOOK_SECRET_KEY"
    option StripeCredentials
            { stripePublicKey = cs stripePublicKey
            , stripeSecretKey = cs stripeSecretKey
            , stripeWebhookSecretKey = cs stripeWebhookSecretKey
            }


-- | Returns the stripe credentials configured with 'initStripe'
stripeCredentials :: (?context :: context, ConfigProvider context) => StripeCredentials
stripeCredentials = ?context
            |> getFrameworkConfig
            |> get #appConfig
            |> TMap.lookup @StripeCredentials
            |> fromMaybe (error "Could not find StripeCredentials in config. Did you forgot to call 'initStripe' inside your Config.hs?")
