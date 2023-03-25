{-|
Module: IHP.Mail.Types
Description: Types for emails
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.Mail.Types
( MailServer (..)
, MailAttachment (..)
, SMTPEncryption (..)
)
where

import IHP.Prelude
import Network.Socket (PortNumber)

-- | Configuration for a mailer used by IHP
data SMTPEncryption = Unencrypted | TLS | STARTTLS

data MailServer =
    -- | Uses AWS SES for sending emails
    SES { accessKey :: ByteString
        , secretKey :: ByteString
        --  | E.g. @"us-east-1"@ or @"eu-west-1"@
        , region :: Text }
    -- | Uses the local Sendmail binary for sending emails. Avoid this with IHP Cloud
    | Sendmail
    -- | Uses SendGrid for sending emails
    | SendGrid { apiKey :: Text
               , category :: Maybe Text }
    -- | Uses a generic SMTP server for sending emails
    | SMTP { host :: String
           , port :: PortNumber
           -- (Username,Password) combination
           , credentials :: Maybe (String, String)
           , encryption :: SMTPEncryption }

data MailAttachment = MailAttachment
    { name :: Text -- ^ File name of an attachment
    , content :: LByteString
    , contentType :: Text
    } deriving (Eq, Show)
