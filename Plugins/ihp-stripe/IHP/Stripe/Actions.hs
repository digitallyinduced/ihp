{-|
Description: Common built-in stripe actions
Copyright: (c) digitally induced GmbH, 2021
-}
module IHP.Stripe.Actions where

import IHP.Prelude
import Data.Aeson
import qualified Data.Maybe as Maybe
import IHP.Stripe.Types
import IHP.Stripe.Request

data CheckoutSession = CheckoutSession
    { id :: Text
    , url :: Text
    }

data CreateCheckoutSession = CreateCheckoutSession
    { successUrl :: Text
    , cancelUrl :: Text
    , mode :: Text
    , paymentMethodTypes :: [Text]
    , customer :: Maybe Text
    , lineItem :: LineItem
    , metadata :: [(Text, Text)]
    }

data LineItem = LineItem
    { price :: Text
    , quantity :: Int
    , taxRate :: Maybe Text
    , adjustableQuantity :: Maybe AdjustableQuantity
    }

data AdjustableQuantity = AdjustableQuantity
    { enabled :: !Bool
    , minimum :: !Int
    , maximum :: !Int
    }

data CreateCustomer = CreateCustomer
    { address :: Address
    , email :: Text
    , name :: Text
    , vatId :: Maybe Text
    , vatType :: Maybe Text
    , taxExempt :: Text
    }
data Customer = Customer
    { id :: Text
    }

data Address = Address
    { line1 :: Text
    , city :: Text
    , country :: Text
    , line2 :: Text
    , postalCode :: Text
    , state :: Text
    }

data CreateBillingPortalSession
    = CreateBillingPortalSession
    { customer :: Text
    , returnUrl :: Text
    } deriving (Eq, Show)

data BillingPortalSession = BillingPortalSession
    { url :: Text
    }

data RetrieveSubscription = RetrieveSubscription
    { id :: Text
    }

data Subscription = Subscription
    { id :: Text
    , customer :: Text
    }

instance FromJSON CheckoutSession where
    parseJSON (Object v) = CheckoutSession <$> v .: "id" <*> v .: "url"

instance FromJSON Customer where
    parseJSON (Object v) = Customer <$> v .: "id"

instance FromJSON BillingPortalSession where
    parseJSON (Object v) = BillingPortalSession <$> v .: "url"

instance FromJSON Subscription where
    parseJSON (Object v) = Subscription <$> v .: "id" <*> v .: "customer"

--instance FromJSON LineItem where
--    parseJSON (Object v) = LineItem <$> v .: "id" <*> v .: "price" <*> v .: "quantity" <*> v .:? "taxRate" <*> (pure Nothing)

instance FromJSON SubscriptionItemsList where
    parseJSON (Object v) = SubscriptionItemsList <$> v .: "data"

instance FromJSON SubscriptionItem where
    parseJSON (Object v) = SubscriptionItem <$> v .: "id" <*> v .: "quantity"

instance StripeAction CreateCheckoutSession where
    type StripeResult CreateCheckoutSession = CheckoutSession
    actionUrl _ = "https://api.stripe.com/v1/checkout/sessions"
    send = postStripeRequest
    requestPayload action@(CreateCheckoutSession { .. }) = 
        let metadataPayload = metadata
                |> map (\(key, value) -> ("metadata[" <> cs key <> "]", cs value))
        in
                    [ ("success_url", cs successUrl)
                    , ("cancel_url", cs cancelUrl)
                    , ("payment_method_types[0]", "card")
                    , ("line_items[0][price]", lineItem |> get #price |> cs)
                    , ("line_items[0][quantity]", lineItem |> get #quantity |> tshow |> cs)
                    , ("automatic_tax[enabled]", "true")
                    , ("tax_id_collection[enabled]", "true")
                    , ("mode", cs mode)
                    ] <>
                    (case customer of
                        Just customer -> [ ("customer", cs customer), ("customer_update[name]", "auto") ]
                        Nothing -> []
                    )
                    <> (case get #adjustableQuantity lineItem of
                        Just adjustableQuantity | get #enabled adjustableQuantity ->
                            [ ("line_items[0][adjustable_quantity][enabled]", "true")
                            , ("line_items[0][adjustable_quantity][minimum]", adjustableQuantity |> get #minimum |> tshow |> cs)
                            , ("line_items[0][adjustable_quantity][maximum]", adjustableQuantity |> get #maximum |> tshow |> cs)
                            ]
                        _ -> []
                    )
                    <> metadataPayload



instance StripeAction CreateCustomer where
    type StripeResult CreateCustomer = Customer
    actionUrl _ = "https://api.stripe.com/v1/customers"
    send = postStripeRequest
    requestPayload action@(CreateCustomer { .. }) = 
                [ ("address[line1]", cs (get #line1 address))
                , ("address[city]", cs (get #city address))
                , ("address[country]", cs (get #country address))
                , ("address[line2]", cs (get #line2 address))
                , ("address[postal_code]", cs (get #postalCode address))
                , ("address[state]", cs (get #state address))
                , ("email", cs email)
                , ("name", cs name)
                , ("tax_exempt", cs taxExempt)
                ]
                <>
                    case vatId of
                        Just vatId ->
                            [ ("tax_id_data[0][type]", cs (Maybe.fromMaybe (error "vatType needs to be set") vatType))
                            , ("tax_id_data[0][value]", cs vatId) ]
                        Nothing -> []



instance StripeAction CreateBillingPortalSession where
    type StripeResult CreateBillingPortalSession = BillingPortalSession
    actionUrl _ = "https://api.stripe.com/v1/billing_portal/sessions"
    send = postStripeRequest
    requestPayload action@(CreateBillingPortalSession { .. }) = 
                [ ("customer", cs customer)
                , ("return_url", cs returnUrl)
                ]

instance StripeAction RetrieveSubscription where
    type StripeResult RetrieveSubscription = Subscription
    actionUrl RetrieveSubscription { id } = "https://api.stripe.com/v1/subscriptions/" <> (cs id)
    send = postStripeRequest
    requestPayload action = []


data UpdateSubscriptionItem = UpdateSubscriptionItem { id :: Text, quantity :: !Int }

instance StripeAction UpdateSubscriptionItem where
    type StripeResult UpdateSubscriptionItem = SubscriptionItem
    actionUrl UpdateSubscriptionItem { .. } = "https://api.stripe.com/v1/subscription_items/" <> cs id
    send = postStripeRequest
    requestPayload action@(UpdateSubscriptionItem { .. }) = [ ("quantity", cs $ tshow quantity) ]


data SubscriptionItemsList = SubscriptionItemsList { data_ :: [SubscriptionItem] }
data SubscriptionItem = SubscriptionItem { id :: Text, quantity :: !Int }
data RetrieveSubscriptionItems = RetrieveSubscriptionItems { subscriptionId :: Text }

instance StripeAction RetrieveSubscriptionItems where
    type StripeResult RetrieveSubscriptionItems = SubscriptionItemsList
    actionUrl RetrieveSubscriptionItems { subscriptionId } = "https://api.stripe.com/v1/subscription_items?subscription=" <> cs subscriptionId
    requestPayload _ = []
    send = getStripeRequest


