module IHP.AuthSupport.Mail.PasswordResets.PasswordResetLink where
import IHP.ViewPrelude
import IHP.MailPrelude

data PasswordResetLinkMail record = PasswordResetLinkMail
    { user :: record
    , token :: Text
    }