{-# LANGUAGE AllowAmbiguousTypes #-}
{-|
Module: IHP.AuthSupport.Controller.Confirmations
Description: Provides action implementation for confirming users
Copyright: (c) digitally induced GmbH, 2021
-}
module IHP.AuthSupport.Controller.Confirmations where

import IHP.Prelude
import IHP.ControllerPrelude
import qualified IHP.AuthSupport.Controller.Sessions as Sessions
import Data.Data
import IHP.AuthSupport.Types
import IHP.ViewSupport (View, Layout)
import IHP.Mail (BuildMail)

confirmAction :: forall record action.
    ( ?theAction :: action
    , ?context :: ControllerContext
    , ?modelContext :: ModelContext
    , ConfirmationsControllerConfig record
    , record ~ GetModelByTableName (GetTableName record)
    , CanUpdate record
    , FilterPrimaryKey (GetTableName record)
    , KnownSymbol (GetModelName record)
    , HasField "id" record (Id record)
    , Show (PrimaryKey (GetTableName record))
    , SetField "confirmationToken" record (Maybe Text)
    , SetField "isConfirmed" record Bool
    , HasField "confirmationToken" record (Maybe Text)
    , HasField "isConfirmed" record Bool
    , Sessions.SessionsControllerConfig record
    ) => Id record -> Text -> IO ()
confirmAction userId confirmationToken = do
    user <- fetch userId
    
    when (get #isConfirmed user) do
        setSuccessMessage "Your account is already confirmed"
        redirectToPath (Sessions.afterLoginRedirectPath @record)

    let expectedConfirmationToken = get #confirmationToken user
    if (Just confirmationToken) == expectedConfirmationToken
        then do
            user
                |> set #isConfirmed True
                |> set #confirmationToken Nothing
                |> updateRecord

            setSuccessMessage "Your account is confirmed now. Let's get started!"
            login @record user
            
            afterConfirmation user

            redirectToPath (Sessions.afterLoginRedirectPath @record)
        else do
            setErrorMessage "Invalid confirmation token provided"
            redirectToPath "/"

class ConfirmationsControllerConfig record where
    -- Called after a user was successfully confirmed. You can e.g. send a welcome email here.
    --
    -- Example: Send a welcome email after the user successfully confirmed his account
    --
    -- > instance Confirmations.ConfirmationsControllerConfig User where
    -- >     afterConfirmation user = do
    -- >         sendMail WelcomeMail { user }
    --
    afterConfirmation :: (?context :: ControllerContext, ?modelContext :: ModelContext) => record -> IO ()
    afterConfirmation user = pure ()