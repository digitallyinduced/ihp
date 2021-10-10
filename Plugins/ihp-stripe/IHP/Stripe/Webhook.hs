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
