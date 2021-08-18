{-|
Module: IHP.AuthSupport.Confirm
Description: Email Confirmation for Sign up (IHP Pro)
Copyright: (c) digitally induced GmbH, 2021
-}
module IHP.AuthSupport.Confirm where

import IHP.ControllerPrelude
import IHP.AuthSupport.Types
import IHP.Mail (BuildMail)

data ConfirmationMail user = ConfirmationMail
    { user :: user
    , confirmationToken :: Text -- ^ While you can get this information from the user field, the user confirmation token is a 'Maybe Text' and then would need to be unwrapped manually
    }

-- | Sends out an account confirmation email to the user.
--
-- The confirmation token is refreshed each time this function is called.
--
-- __Example: Send a confirmation mail to bob@example.com__
--
-- > user <- newRecord @User
-- >         |> set #email "bob@example.com"
-- >         |> createRecord
-- >
-- > sendConfirmationMail user
--
sendConfirmationMail ::
    ( ?context :: ControllerContext
    , ?modelContext :: ModelContext
    , BuildMail (ConfirmationMail user)
    , CanUpdate user
    , SetField "confirmationToken" user (Maybe Text)
    ) => user -> IO ()
sendConfirmationMail user = do
    confirmationToken <- generateAuthenticationToken
    user
        |> setJust #confirmationToken confirmationToken
        |> updateRecord

    sendMail ConfirmationMail { .. }

-- | Should be called from 'SessionsControllerConfig.beforeLogin' to stop logins when the user is not yet confirmed
--
-- __Example:__
--
-- > import qualified IHP.AuthSupport.Confirm as Confirm
-- > 
-- > instance Sessions.SessionsControllerConfig User where
-- >     beforeLogin user = do
-- >         Confirm.ensureIsConfirmed user
--
ensureIsConfirmed :: (?context :: ControllerContext, HasField "isConfirmed" user Bool) => user -> IO ()
ensureIsConfirmed user = do
    unless (get #isConfirmed user) do
        setErrorMessage "Please click the confirmation link we sent to your email before you can use this account"
        redirectBack