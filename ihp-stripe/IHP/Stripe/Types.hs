{-|
Description: Data structures for the IHP Stripe Integration
Copyright: (c) digitally induced GmbH, 2021
-}
module IHP.Stripe.Types where

import IHP.Prelude
import IHP.FrameworkConfig

-- | Stored inside the controller context
data StripeCredentials = StripeCredentials
    { stripePublicKey :: !Text
    , stripeSecretKey :: !Text
    , stripeWebhookSecretKey :: !Text
    }

-- | A stripe action consists of the following elements
--
-- 1. A data structure for the request, the 'action' type
-- 2. A data structure for the result, the 'StripeResult action'
-- 3. A function 'actionUrl' that returns the url to the stripe resource
-- 4. The 'send' function which actually delivers the request, typically 'postStripeRequest' or 'getStripeRequest'
-- 5. The 'requestPayload' function which returns all POST fields for the request
class StripeAction action where
    -- | The data structure into which the response should be parsed into.
    --
    -- > data BillingPortalSession = BillingPortalSession { url :: Text }
    -- >
    -- > instance FromJSON BillingPortalSession where
    -- >     parseJSON (Object v) = BillingPortalSession <$> v .: "url"
    -- >
    -- > type StripeResult CreateBillingPortalSession = BillingPortalSession
    --
    type StripeResult action :: *

    -- | Returns the request url
    --
    -- > actionUrl RetrieveSubscription { id } = "https://api.stripe.com/v1/subscriptions/" <> (cs id)
    --
    actionUrl :: action -> String

    -- | Typically set to 'postStripeRequest' for a POST request or 'getStripeRequest' for a GET request
    --
    -- > send = postStripeRequest
    --
    send :: (?context :: context, ConfigProvider context) => action -> IO (StripeResult action)
    
    -- Returns the POST parameters. For GET requests this is ignored and should return an empty list.
    --
    -- > requestPayload UpdateSubscriptionItem { quantity } = [ ("quantity", cs $ tshow quantity) ]
    --
    requestPayload :: action -> [(ByteString, ByteString)]

data StripeWebhookController
    = StripeWebhookAction
    deriving (Eq, Show, Data)