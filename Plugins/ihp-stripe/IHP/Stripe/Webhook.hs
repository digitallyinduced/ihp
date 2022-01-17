module IHP.Stripe.Webhook where

import IHP.Prelude
import qualified Data.Aeson as Aeson
import Data.Time.Clock.POSIX
import qualified Data.Map as Map

import qualified Stripe.Signature as Stripe
import qualified Stripe.Concepts as Stripe

import IHP.Stripe.Types
import IHP.Stripe.Config
import IHP.ControllerPrelude

import qualified IHP.Log.Types as Log
import qualified IHP.Log as Log

-- See https://stripe.com/docs/api/events/types
data StripeEvent
    = CheckoutSessionCompleted
        { checkoutSessionId :: Text
        , subscriptionId :: Text
        , customer :: Text -- ^ The stripe customer id
        , metadata :: [(Text, Text)]
        } 
    | InvoiceFinalized
        { subscriptionId :: Text
        , stripeInvoiceId :: Text
        , invoiceUrl :: Text
        , invoicePdf :: Text
        , createdAt :: UTCTime
        , total :: Integer
        , currency :: Text
        }
    | CustomerSubscriptionUpdated -- ^ Occurs whenever a subscription changes (e.g., switching from one plan to another, or changing the status from trial to active).
        { subscription :: Subscription
        }
    | CustomerSubscriptionDeleted -- ^ Occurs whenever a customer's subscription ends.
        { subscriptionId :: Text
        }
    | CustomerSubscriptionTrialWillEnd -- ^ Occurs three days before a subscription's trial period is scheduled to end, or when a trial is ended immediately (using trial_end=now).
        { subscriptionId :: Text
        , trialEnds :: UTCTime
        , cancelAtPeriodEnd :: Bool
        }
    | OtherEvent


instance FromJSON StripeEvent where
    parseJSON (Object event) = do
        type_ :: Text <- event .: "type"
        payload <- event .: "data"
        case type_ of
            "checkout.session.completed" -> do
                checkoutSession <- payload .: "object"
                checkoutSessionId :: Text <- checkoutSession .: "id"
                customer :: Text <- checkoutSession .: "customer"
                subscriptionId :: Text <- checkoutSession .: "subscription"
                metadata :: Map.Map Text Text <- checkoutSession .: "metadata"
                pure CheckoutSessionCompleted { checkoutSessionId, subscriptionId, metadata = Map.toList metadata, customer }
            "invoice.finalized" -> do
                invoice <- payload .: "object"
                subscriptionId :: Text <- invoice .: "subscription"
                stripeInvoiceId :: Text <- invoice .: "id"
                invoiceUrl :: Text <- invoice .: "hosted_invoice_url"
                invoicePdf :: Text <- invoice .: "invoice_pdf"
                created :: Integer <- invoice .: "created"
                total :: Integer <- invoice .: "total"
                currency :: Text <- invoice .: "currency"
                let createdAt :: UTCTime = posixSecondsToUTCTime (fromInteger created)
                pure InvoiceFinalized { subscriptionId, stripeInvoiceId, invoiceUrl, invoicePdf, createdAt, total, currency }
            "customer.subscription.updated" -> do
                subscription <- payload .: "object"
                pure CustomerSubscriptionUpdated { .. }
            "customer.subscription.deleted" -> do
                subscription <- payload .: "object"
                subscriptionId :: Text <- subscription .: "id"

                pure CustomerSubscriptionDeleted { .. }
            "customer.subscription.trial_will_end" -> do
                subscription <- payload .: "object"
                subscriptionId :: Text <- subscription .: "id"
                currentEndPeriod :: POSIXTime <- subscription .: "current_period_start"
                let trialEnds :: UTCTime = posixSecondsToUTCTime currentEndPeriod
                cancelAtPeriodEnd :: Bool <- subscription .: "cancel_at_period_end"
                pure CustomerSubscriptionTrialWillEnd { .. }
            _ -> pure OtherEvent

instance StripeEventController => Controller StripeWebhookController where
    action StripeWebhookAction = do
        payload <- getRequestBody
        ensureIsStripe payload

        -- It's useful to see stripe requests in development
        when isDevelopment do
            Log.info payload

        let event = case Aeson.eitherDecode (cs payload) of
                Left message -> error (cs message)
                Right value -> value
        
        IHP.Stripe.Webhook.on event

        renderPlain ""

class StripeEventController where
    on :: (?context :: ControllerContext, ?modelContext :: ModelContext) => StripeEvent -> IO ()

-- | Ensure that the request is from stripe. Aborts if the signature does not match.
ensureIsStripe :: (?context :: ControllerContext) => LByteString -> IO ()
ensureIsStripe requestBody = do
    let signatureHeader = getHeader "Stripe-Signature" |> fromMaybe (error "Stripe-Signature header missing")

    let signature = Stripe.parseSig (cs signatureHeader) |> fromMaybe (error "Cannot parse stripe signature")
    let isValid = Stripe.isSigValid signature (stripeCredentials |> get #stripeWebhookSecretKey |> Stripe.textToWebhookSecretKey) (cs requestBody)
    accessDeniedUnless isValid
