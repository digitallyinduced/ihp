# Stripe

```toc

```

## Introduction

You can use the IHP Stripe Integration to accept payments within your IHP application.

The IHP Stripe integration is available [with IHP Pro and IHP Business](ihp-pro.html). If you're not on IHP Pro yet, now is a good time to try it out. By switching to Pro, you're supporting the sustainable development of IHP.

This guide asumes you've read through the basics of the stripe documentation to get a gist of how the stripe api works.

## Setup

### Install ihp-stripe in your IHP app

Add `ihp-stripe` to the `haskellDeps` in your `default.nix`:

```nix
let
    ...
    haskellEnv = import "${ihp}/NixSupport/default.nix" {
        ihp = ihp;
        haskellDeps = p: with p; [
            # ...
            ihp-stripe
        ];
    ...
```

Now you need to remake your environment using `make -B .envrc`.


Next add `import IHP.Stripe.Config` to your `Config/Config.hs`:
```haskell
module Config where

-- ...

import IHP.Stripe.Config
```


Add a call to `initStripe` inside the `Config/Config.hs` to configure the stripe credentials:

```haskell
module Config where

import IHP.Prelude
import IHP.Environment
import IHP.FrameworkConfig
import IHP.Stripe.Config

config :: ConfigBuilder
config = do
    option Development
    option (AppHostname "localhost")

    initStripe
```

### Stripe API Keys

Open the `start` script and add the following env variables:

```bash
# Add these before the `RunDevServer` call at the end of the file
export STRIPE_WEBHOOK_SECRET_KEY="whsec_..."
export STRIPE_PUBLIC_KEY="pk_test_..."
export STRIPE_SECRET_KEY=""

# Finally start the dev server
RunDevServer
```

Replace the `whsec_...` and `pk_test_...` with your stripe api keys. It's best to use test-mode keys here for now. [You can get them on the stripe dashboard](https://dashboard.stripe.com/test/apikeys).

**Keep the `STRIPE_SECRET_KEY` empty for now. We'll get back to that in the next steps.**

## Subscriptions

In this guide we'll deal with how to set up a typical SaaS subscription. We also asume you're using [stripe tax to collect VAT](https://stripe.com/en-de/tax).

### Plans

Depending on your project you might have multiple pricing plans.

Even if you have only a single plan for now, it's helpful to model this in our application with a `plans` table.

Open the `Application/Schema.sql` and add this:

```sql 
CREATE TABLE plans (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    name TEXT NOT NULL,
    stripe_price_id TEXT NOT NULL
);
```

Next go to the stripe dashboard and [a new product](https://support.stripe.com/questions/how-to-create-products-and-prices). Inside the product you need to add prices.

After you've created your product and a first price, [insert a first plan into the database using the data editor](http://localhost:8001/ShowDatabase). Make sure to fill the `stripe_price_id` with the right stripe price id (looks like `price_...`).

### Subscriptions Table

Open `Application/Schema.sql` and add this `subscriptions` table:

```sql
CREATE TABLE subscriptions (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    user_id UUID NOT NULL,
    starts_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    ends_at DATE DEFAULT NULL,
    is_active BOOLEAN DEFAULT true NOT NULL,
    stripe_subscription_id TEXT NOT NULL,
    plan_id UUID NOT NULL,
    quantity INT DEFAULT 1 NOT NULL
);

ALTER TABLE subscriptions ADD CONSTRAINT subscriptions_ref_plan_id FOREIGN KEY (plan_id) REFERENCES plans (id) ON DELETE NO ACTION;
ALTER TABLE subscriptions ADD CONSTRAINT subscriptions_ref_user_id FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE NO ACTION;
```

Additionally you also need to add the following fields to your `users` table:

```sql
CREATE TABLE users (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    -- ...,

    stripe_customer_id TEXT DEFAULT NULL,
    plan_id UUID DEFAULT NULL,
    subscription_id UUID DEFAULT NULL
);

CREATE INDEX users_stripe_customer_id_index ON users (stripe_customer_id);
CREATE INDEX users_plan_id_index ON users (plan_id);
CREATE INDEX users_subscription_id_index ON users (subscription_id);

ALTER TABLE users ADD CONSTRAINT users_ref_plan_id FOREIGN KEY (plan_id) REFERENCES plans (id) ON DELETE NO ACTION;
ALTER TABLE users ADD CONSTRAINT users_ref_subscription_id FOREIGN KEY (subscription_id) REFERENCES subscriptions (id) ON DELETE NO ACTION;
```

### Checkout Sessions

In stripe a payment process for a subscription is started by creating a checkout session and redirecting the user to the checkout session page.

For dealing with the checkout sessions, we'll create a new `CheckoutSessions` controller. Open `Web/Types.hs` and add this:

```haskell
data CheckoutSessionsController
    = CheckoutSuccessAction
    | CheckoutCancelAction
    | CreateCheckoutSessionAction
    deriving (Eq, Show, Data)
```

Next create the `Web/Controller/CheckoutSessions.hs` file with this content:

```haskell
module Web.Controller.CheckoutSessions where

import Web.Controller.Prelude

import qualified IHP.Stripe.Types as Stripe
import qualified IHP.Stripe.Actions as Stripe

instance Controller CheckoutSessionsController where
    beforeAction = ensureIsUser

    action CreateCheckoutSessionAction = do
        -- You can later customize this, e.g. you could pass
        -- the planId via a form on the pricing page
        plan <- query @Plan |> fetchOne

        stripeCheckoutSession <- Stripe.send Stripe.CreateCheckoutSession
                { successUrl = urlTo CheckoutSuccessAction
                , cancelUrl = urlTo CheckoutCancelAction
                , mode = "subscription"
                , paymentMethodTypes = ["card"]
                , customer = get #stripeCustomerId currentUser
                , lineItem = Stripe.LineItem
                    { price = get #stripePriceId plan
                    , quantity = 1
                    , taxRate = Nothing
                    , adjustableQuantity = Nothing
                    }
                , metadata =
                    [ ("userId", tshow currentUserId)
                    , ("planId", tshow get #id plan)
                    ]
                }

        redirectToUrl (get #url stripeCheckoutSession)

    action CheckoutSuccessAction = do
        plan <- fetchOne (get #planId currentUser)
        setSuccessMessage ("You're on the " <> get #name plan <> " plan now!")

        -- To keep things simple we just redirect the user to the app's start page
        -- after successful subscribing to our plan.
        --
        -- It's best to have a dedicated "payment success" page, where
        -- this action then should redirect to.
        redirectToPath "/"
    
    action CheckoutCancelAction = do
        -- You typically want to redirect the user to the page where the payment process
        -- was started. E.g. `redirectTo PricingAction`.
        --
        -- To keep things simple in the Guide, we redirect to the start page
        -- of the app for now.
        redirectToPath "/"
```

Open `Web/Routes.hs` and enable routing:

```haskell
instance AutoRoute CheckoutSessionsController
```

Open `Web/FrontController.hs` and enable the new controller:

```haskell
-- ADD THIS IMPORT:
import Web.Controller.CheckoutSessions

instance FrontController WebApplication where
    controllers = 
        [ startPage StartpageAction
        -- ...

        -- ADD THIS:
        , parseRoute @CheckoutSessionsController
        ]
```

### Starting a Checkout Session

You should now be able to start a checkout session. For doing that we need to place a `Payment` button somewhere in your app. The recommended approach is to create a new `PricingAction` inside your `StaticController` and then have the following form there:

```html
<form method="POST" action={CreateCheckoutSessionAction} data-disable-javascript-submission={True}>
    <button class="btn btn-primary">Subscribe to Plan</button>
</form>
```

After you've added the button, you can give it a try. Submitting the form will call the `CreateCheckoutSessionAction`, which will redirect to the stripe payment page.

### Receiving Webhooks

To be notified about a new successful subscription to one of our products, we'll need to set up Webhooks.

Create a new file `Web/Controller/StripeWebhook.hs` with this content:

```haskell
module Web.Controller.StripeWebhook where

import Web.Controller.Prelude

import IHP.Stripe.Actions
import IHP.Stripe.Webhook
import IHP.Stripe.Types

instance StripeEventController where
    on CheckoutSessionCompleted { checkoutSessionId, subscriptionId, customer, metadata } = do
        let userId :: Maybe (Id User) = metadata
                |> lookup "userId"
                |> fmap textToId
        let planId :: Id Plan = metadata
                |> lookup "planId"
                |> fmap textToId
                |> fromMaybe (error "Requires plan id")

        user <- fetchOneOrNothing userId
        case user of
            Just user -> do
                subscription <- newRecord @Web.Controller.Prelude.Subscription
                    |> set #userId (get #id user)
                    |> set #stripeSubscriptionId subscriptionId
                    |> set #planId planId
                    |> set #quantity 1
                    |> createRecord

                user
                    |> setJust #subscriptionId (get #id subscription)
                    |> setJust #planId planId
                    |> setJust #stripeCustomerId customer
                    |> updateRecord

                pure ()
            Nothing -> do
                putStrLn "Stripe CheckoutSessionCompleted: CheckoutSession not found."
                pure ()

    on InvoiceFinalized { subscriptionId, stripeInvoiceId, invoiceUrl, invoicePdf, createdAt, total, currency } = do 
        pure () -- We'll handle this later
    on OtherEvent = do
        putStrLn "Skipping OtherEvent"
```

Next we need to load the routing. Open `Web/Routes.hs` and add this import:

```haskell
import IHP.Stripe.Routes
```

Finally we need to hook this into the FrontController. Open `Web/FrontController.hs` and add this:

```haskell
-- ADD THESE IMPORTS:
import Web.Controller.StripeWebhook
import IHP.Stripe.Types

instance FrontController WebApplication where
    controllers = 
        [ startPage StartpageAction
        -- ...
        , parseRoute @SubscriptionsController

        -- ADD THIS:
        , parseRoute @StripeWebhookController
        ]

```

Now webhooks are ready.

To test it out [install the Stripe CLI](https://stripe.com/docs/stripe-cli). Run `stripe listen --forward-to localhost:8000/StripeWebhook` to forward test-mode stripe events to your app.

On the first start this command will print out a webhook secret key (starting with `whsec_`). Copy the key and open `start`. Inside the `start` script set the `export STRIPE_WEBHOOK_SECRET_KEY=""` line to this key. After this restart your app.

### Handling Unsubscribes

To automatically deal with customers that unsubscribe, add the following handler to `Web/Controller/StripeWebhook.hs`:

```haskell
on CustomerSubscriptionUpdated { subscriptionId, cancelAtPeriodEnd, currentPeriodEnd } = do
    maybeSubscription <- query @Subscription
            |> filterWhere (#stripeSubscriptionId, subscriptionId)
            |> fetchOneOrNothing
    case maybeSubscription of
        Just subscription -> do
            subscription 
                |> set #endsAt (if cancelAtPeriodEnd
                        then currentPeriodEnd
                        else Nothing)
                |> updateRecord
            pure ()
        Nothing -> pure ()

on CustomerSubscriptionDeleted { subscriptionId } = do
    maybeSubscription <- query @Subscription
            |> filterWhere (#stripeSubscriptionId, subscriptionId)
            |> fetchOneOrNothing
    case maybeSubscription of
        Just subscription -> do
            now <- getCurrentTime
            subscription 
                |> set #endsAt now
                |> set #isActive False
                |> updateRecord

            user <- fetch (get #userId subscription)
            user
                |> set #planId Nothing
                |> set #subscriptionId Nothing
                |> updateRecord
        Nothing -> pure ()
````

This will set the `endsAt` date on the subscription, will mark the subscription as not active anymore and then sets the `planId` of the user to `null`.

## Billing Portal

To integrate the Stripe Billing Portal, add an action like this to your app:

```haskell
-- Add these imports at the top
import qualified IHP.Stripe.Actions as Stripe
import qualified IHP.Stripe.Types as Stripe

-- Then add this action:
    action OpenBillingPortalAction = do
        subscription <- currentUser
            |> get #subscriptionId
            |> fetchOne

        stripeSubscription <- Stripe.send Stripe.RetrieveSubscription { id = get #stripeSubscriptionId subscription }

        billingPortal <- Stripe.send Stripe.CreateBillingPortalSession
                { customer = get #customer stripeSubscription
                , returnUrl = urlTo StartpageAction -- <- You might need to customize the return url here
                }

        billingPortal
            |> get #url
            |> redirectToUrl
```

Use a form to link to the billing portal:

```html
<form method="POST" action={OpenBillingPortalAction} data-disable-javascript-submission={True}>
    <button type="submit" class="btn btn-primary">Change Payment Details</button>
</form>
```

## Invoices

While your customers can view their invoices using the stripe billing portal, you still might want to integrate them into your app.

### Data Structures

Open `Application/Schema.sql` and add a `invoices` table:

```sql
CREATE TABLE invoices (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    subscription_id UUID NOT NULL,
    stripe_invoice_id TEXT NOT NULL,
    invoice_url TEXT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE NOT NULL,
    invoice_pdf TEXT NOT NULL,
    total INT NOT NULL,
    currency TEXT NOT NULL
);
ALTER TABLE invoices ADD CONSTRAINT invoices_ref_subscription_id FOREIGN KEY (subscription_id) REFERENCES subscriptions (id) ON DELETE NO ACTION;
```

### Webhook Handler

Next open `Web/Controller/StripeWebhook.hs` and implement the `on InvoiceFinalized`:

```haskell
    on InvoiceFinalized { subscriptionId, stripeInvoiceId, invoiceUrl, invoicePdf, createdAt, total, currency } = do
        -- The subscription could not be here if it's e.g. created by some other app in your stripe account
        -- or when it's added by-hand
        subscriptionMaybe <- query @Subscription
            |> filterWhere (#stripeSubscriptionId, subscriptionId)
            |> fetchOneOrNothing

        case subscriptionMaybe of
            Just subscription -> do
                existingInvoice <- query @Invoice
                    |> filterWhere (#stripeInvoiceId, stripeInvoiceId)
                    |> fetchOneOrNothing

                case existingInvoice of
                    Just invoice -> do
                        invoice
                            |> set #invoiceUrl invoiceUrl
                            |> set #createdAt createdAt
                            |> set #invoicePdf invoicePdf
                            |> set #total (fromInteger total)
                            |> set #currency currency
                            |> updateRecord
                        pure ()
                    Nothing -> do
                        invoice <- newRecord @Invoice
                            |> set #subscriptionId (get #id subscription)
                            |> set #stripeInvoiceId stripeInvoiceId
                            |> set #invoiceUrl invoiceUrl
                            |> set #createdAt createdAt
                            |> set #invoicePdf invoicePdf
                            |> set #total (fromInteger total)
                            |> set #currency currency
                            |> createRecord
                        pure ()
                pure ()
            Nothing -> do
                putStrLn "Stripe InvoiceFinalized: Subscription not found."
                pure ()
```

Now stripe invoices are added to the `invoices` table whenever they're available.

### Showing Invoices

To make the invoices available to your users, create a `Invoices` controller. In the `InvoicesAction` you can query the invoices for a user like this:

```haskell
    action InvoicesAction = do
        subscriptions <- query @Subscription
            |> filterWhere (#userId, currentUserId)
            |> fetch

        invoices <- query @Invoice
            |> orderByDesc #createdAt
            |> filterWhereIn (#subscriptionId, ids subscriptions)
            |> fetch

        render IndexView { .. }
```

Here's an example on how to render the invoices in your view:

```haskell
renderInvoice :: Invoice -> Html
renderInvoice invoice = [hsx|
    <div class="card d-flex flex-row py-3 px-1 mb-1">
        <div class="col">
            <a>{get #createdAt invoice |> date}</a>
        </div>
        <div class="col">
            <a href={get #invoiceUrl invoice} target="_blank">Subscription</a>
        </div>
        <div class="col">
            <a>{renderPrice (get #currency invoice) (get #total invoice)}</a>
        </div>
        <div class="col-xs mr-3">
            <a href={get #invoicePdf invoice}>Download</a>
        </div>
    </div>
|]

renderPrice :: Text -> Int -> Text
renderPrice "eur" amount = show (fromIntegral(amount)/100) <> "€"
renderPrice "usd" amount = "$" <> show (fromIntegral(amount)/100)
renderPrice code amount = show (fromIntegral(amount)/100) <> toUpper code
```
