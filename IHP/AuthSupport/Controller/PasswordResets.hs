{-# LANGUAGE AllowAmbiguousTypes #-}
module IHP.AuthSupport.Controller.PasswordResets where

import IHP.Prelude
import IHP.ControllerPrelude hiding (Success, currentUserOrNothing)

import IHP.AuthSupport.View.PasswordResets.New
import IHP.AuthSupport.View.PasswordResets.Show
import IHP.AuthSupport.Mail.PasswordResets.PasswordResetLink
import qualified IHP.AuthSupport.Controller.Sessions as Sessions
import Data.Data
import IHP.AuthSupport.Types
import IHP.ViewSupport (View, Layout)
import IHP.Mail (BuildMail)

newPasswordResetAction :: forall record action.
    ( ?theAction :: action
    , ?context :: ControllerContext
    , ?modelContext :: ModelContext
    , View (NewView record)
    , Data action
    , Record record
    , HasPath action
    , PasswordResetsControllerConfig record
    ) => IO ()
newPasswordResetAction = do
    let passwordReset = newRecord @PasswordReset
    render (NewView { .. } :: NewView record)

showPasswordResetAction :: forall record action.
    ( ?theAction :: action
    , ?context :: ControllerContext
    , ?modelContext :: ModelContext
    , View (ShowView record)
    , PasswordResetsControllerConfig record
    , record ~ GetModelByTableName (GetTableName record)
    , HasField "passwordResetToken" record (Maybe Text)
    , KnownSymbol (GetTableName record)
    , FromRow record
    , FilterPrimaryKey (GetTableName record)
    ) => Id record -> Text -> IO ()
showPasswordResetAction userId token = do
        user <- fetch userId
        accessDeniedUnless (get #passwordResetToken user == Just token)

        render (ShowView { .. } :: ShowView record)

updatePasswordAction :: forall record action.
    ( ?theAction :: action
    , ?context :: ControllerContext
    , HasNewSessionUrl record
    , ?modelContext :: ModelContext
    , View (NewView record)
    , PasswordResetsControllerConfig record
    , record ~ GetModelByTableName (GetTableName record)
    , CanUpdate record
    , SetField "passwordResetToken" record (Maybe Text)
    , HasField "passwordResetToken" record (Maybe Text)
    , SetField "passwordHash" record Text
    , FromRow record
    , FilterPrimaryKey (GetTableName record)
    , KnownSymbol (GetTableName record)
    ) => Id record -> Text -> IO ()
updatePasswordAction userId token = do
    user <- fetch userId
    accessDeniedUnless (get #passwordResetToken user == Just token)

    user <- fetch userId

    let password = param @Text "password"
    when (isEmpty password) do
        setErrorMessage "Password cannot be empty"
        redirectBack
        
    hashed <- hashPassword password
    user
        |> set #passwordHash hashed
        |> set #passwordResetToken Nothing
        |> updateRecord
    
    setSuccessMessage "Your password has been reset. Please log in with your new credentials."

    redirectToPath (newSessionPath @record)

createPasswordResetAction :: forall record action passwordField.
    (?theAction :: action
    , ?context :: ControllerContext
    , ?modelContext :: ModelContext
    , HasField "email" record Text
    , PasswordResetsControllerConfig record
    , CanUpdate record
    , View (NewView record)
    , record ~ GetModelByTableName (GetTableName record)
    , BuildMail (PasswordResetLinkMail record)
    , SetField "passwordResetToken" record (Maybe Text)
    , FromRow record
    , KnownSymbol (GetTableName record)
    ) => IO ()
createPasswordResetAction = do
    let passwordReset = newRecord @PasswordReset
    passwordReset
        |> fill @'["email"]
        |> validateField #email isEmail
        |> ifValid \case
            Left passwordReset -> render (NewView { .. }  :: NewView record)
            Right passwordReset -> do
                maybeUser <- query @record
                    |> filterWhereCaseInsensitive (#email, get #email passwordReset)
                    |> fetchOneOrNothing

                case maybeUser of
                    Just user -> do
                        token <- generateAuthenticationToken
                        user
                            |> setJust #passwordResetToken token
                            |> updateRecord

                        sendMail PasswordResetLinkMail { user, token }

                        pure ()
                    Nothing -> pure ()
                
                setSuccessMessage "We've sent you an email with steps to reset your password."
                redirectToPath (newSessionPath @record)


class PasswordResetsControllerConfig record where
    -- Default: pathTo NewSessionAction
    newSessionPath :: Text