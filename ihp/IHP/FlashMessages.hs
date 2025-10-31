{-|
Module: IHP.FlashMessages
Description: Success and error messages for your application
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.FlashMessages where

import IHP.Prelude
import IHP.Controller.Context
import IHP.Controller.RequestContext
import IHP.Controller.Session
import qualified Data.Maybe as Maybe
import qualified Network.Wai.Middleware.FlashMessages as FlashMessages
import Network.Wai.Middleware.FlashMessages (FlashMessage (..))
import Network.Wai
import qualified Data.Vault.Lazy as Vault
import System.IO.Unsafe (unsafePerformIO)
import qualified Text.Blaze.Html5 as Html5
import IHP.ViewSupport
import IHP.View.Types
import IHP.Controller.Context

-- | Sets a flash messages. This is shown to the user when the next view is rendered.
--
-- Will be rendered in a bootstrap alert, with the @alert-success@ styling.
-- Take a look at https://getbootstrap.com/docs/4.5/components/alerts/ for how this will look like.
--
-- This requires 'IHP.ViewSupport.renderFlashMessages' to be placed somewhere in the layout or page of the next view.
-- For example:
--
-- > myLayout view = [hsx|
-- >     {renderFlashMessages}
-- >     <main>{view}</main>
-- > |]
setSuccessMessage :: (?context :: ControllerContext) => Text -> IO ()
setSuccessMessage = FlashMessages.setSuccessMessage sessionVaultKey ?context.requestContext.request

-- | Sets a flash messages. This is shown to the user when the next view is rendered.
--
-- Will be rendered in a bootstrap alert, with the @alert-danger@ styling.
-- Take a look at https://getbootstrap.com/docs/4.5/components/alerts/ for how this will look like.
--
-- This requires 'IHP.ViewSupport.renderFlashMessages' to be placed somewhere in the layout or page of the next view.
-- For example:
--
-- > myLayout view = [hsx|
-- >     {renderFlashMessages}
-- >     <main>{view}</main>
-- > |]
setErrorMessage :: (?context :: ControllerContext) => Text -> IO ()
setErrorMessage = FlashMessages.setErrorMessage sessionVaultKey ?context.requestContext.request

-- | Returns the flash message currently set
getSuccessMessage :: (?context :: ControllerContext) => IO (Maybe Text)
getSuccessMessage = FlashMessages.getSuccessMessage sessionVaultKey ?context.requestContext.request

-- | Removes the current flash message
clearSuccessMessage :: (?context :: ControllerContext) => IO ()
clearSuccessMessage = FlashMessages.clearSuccessMessage sessionVaultKey ?context.requestContext.request

flashVaultKey :: Vault.Key [FlashMessage]
flashVaultKey = unsafePerformIO Vault.newKey
{-# NOINLINE flashVaultKey #-}

consumeFlashMessagesMiddleware = FlashMessages.consumeFlashMessagesMiddleware sessionVaultKey flashVaultKey

requestFlashMessages :: Request -> [FlashMessage]
requestFlashMessages request =
    fromMaybe (error "consumeFlashMessagesMiddleware was not called") $ FlashMessages.requestFlashMessages flashVaultKey request

-- | Displays the flash messages for the current request.
--
-- You can add a flash message to the next request by calling 'IHP.FlashMessages.setSuccessMessage' or 'IHP.FlashMessages.setErrorMessage':
--
-- > action CreateProjectAction = do
-- >     ...
-- >     setSuccessMessage "Your project has been created successfully"
-- >     redirectTo ShowProjectAction { .. }
--
--
-- > action CreateTeamAction = do
-- >     unless userOnPaidPlan do
-- >         setErrorMessage "This requires you to be on the paid plan"
-- >         redirectTo NewTeamAction
-- >
-- >     ...
--
-- For success messages, the text message is wrapped in a @<div class="alert alert-success">...</div>@, which is automatically styled by bootstrap.
-- Errors flash messages are wraped in @<div class="alert alert-danger">...</div>@.
renderFlashMessages :: (?context :: ControllerContext) => Html5.Html
renderFlashMessages = render theFlashMessages
    where
        render :: [FlashMessage] -> Html5.Html
        render = fromCSSFramework #styledFlashMessages

theFlashMessages :: (?context :: ControllerContext) => [FlashMessage]
theFlashMessages = requestFlashMessages theRequest