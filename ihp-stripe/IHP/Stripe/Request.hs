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

postStripeRequest ::
    ( FromJSON (StripeResult action)
    , StripeAction action
    , ?context :: context
    , ConfigProvider context
    ) => action -> IO (StripeResult action)
postStripeRequest action = do
        response <- Wreq.asJSON =<< (Wreq.postWith stripeOptions (actionUrl action) (requestPayload action))
        pure (response ^. Wreq.responseBody)

getStripeRequest ::
    ( FromJSON (StripeResult action)
    , StripeAction action
    , ?context :: context
    , ConfigProvider context
    ) => action -> IO (StripeResult action)
getStripeRequest action = do
        response <- Wreq.asJSON =<< (Wreq.getWith stripeOptions (actionUrl action))
        pure (response ^. Wreq.responseBody)

stripeOptions :: (?context :: context, ConfigProvider context) => Wreq.Options
stripeOptions = Wreq.defaults & Wreq.auth ?~ (Wreq.basicAuth secretKey "")
    where
        secretKey = stripeCredentials
                |> get #stripeSecretKey
                |> cs
