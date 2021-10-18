{-|
Module: IHP.FlashMessages.ControllerFunctions
Description: Success and error messages for your application
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.FlashMessages.ControllerFunctions where

import IHP.Prelude
import IHP.FlashMessages.Types
import IHP.Controller.Context
import IHP.Controller.Session
import qualified Data.Maybe as Maybe

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
setSuccessMessage = setSession successMessageKey

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
setErrorMessage = setSession errorMessageKey

-- | Returns the flash message currently set
getSuccessMessage :: (?context :: ControllerContext) => IO (Maybe Text)
getSuccessMessage = getSession successMessageKey

-- | Removes the current flash message
clearSuccessMessage :: (?context :: ControllerContext) => IO ()
clearSuccessMessage = setSession successMessageKey ("" :: Text)

-- Returns a list of all flash messages which need to be displayed to the user
--
-- Then clears the flash messages so they won't be displayed again.
getAndClearFlashMessages :: (?context :: ControllerContext) => IO [FlashMessage]
getAndClearFlashMessages = do
    successMessage <- getSessionAndClear successMessageKey
    errorMessage <- getSessionAndClear errorMessageKey
    pure $ Maybe.catMaybes ((fmap SuccessFlashMessage successMessage):(fmap ErrorFlashMessage errorMessage):[])

successMessageKey :: ByteString
successMessageKey = "flashSuccessMessage"

errorMessageKey :: ByteString
errorMessageKey = "flashErrorMessage"

initFlashMessages :: (?context :: ControllerContext) => IO ()
initFlashMessages = do
    flashMessages <- getAndClearFlashMessages
    putContext flashMessages
