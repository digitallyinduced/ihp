{-|
Description: Functions to send requests to the stripe API
Copyright: (c) digitally induced GmbH, 2021
-}
module IHP.Stripe.Request where

import IHP.Prelude
import Data.Aeson
import qualified Network.Wreq as Wreq
import IHP.Stripe.Types
import qualified Data.TMap as TMap
import IHP.FrameworkConfig
import Control.Lens hiding ((|>))
import IHP.Stripe.Config

postStripeRequestWith ::
    ( FromJSON (StripeResult action)
    , StripeAction action
    ) => StripeCredentials -> action -> IO (StripeResult action)
postStripeRequestWith credentials action = do
        let options = Wreq.defaults & Wreq.auth ?~ Wreq.basicAuth (cs credentials.stripeSecretKey) ""
        response <- Wreq.asJSON =<< Wreq.postWith options (actionUrl action) (requestPayload action)
        pure (response ^. Wreq.responseBody)

getStripeRequestWith ::
    ( FromJSON (StripeResult action)
    , StripeAction action
    ) => StripeCredentials -> action -> IO (StripeResult action)
getStripeRequestWith credentials action = do
        let options = Wreq.defaults & Wreq.auth ?~ Wreq.basicAuth (cs credentials.stripeSecretKey) ""
        response <- Wreq.asJSON =<< Wreq.getWith options (actionUrl action)
        pure (response ^. Wreq.responseBody)

postStripeRequest ::
    ( FromJSON (StripeResult action)
    , StripeAction action
    , ?context :: context
    , ConfigProvider context
    ) => action -> IO (StripeResult action)
postStripeRequest action = postStripeRequestWith stripeCredentials action

getStripeRequest ::
    ( FromJSON (StripeResult action)
    , StripeAction action
    , ?context :: context
    , ConfigProvider context
    ) => action -> IO (StripeResult action)
getStripeRequest action = getStripeRequestWith stripeCredentials action
