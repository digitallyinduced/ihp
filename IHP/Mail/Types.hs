{-|
Module: IHP.Mail.Types
Description: Types for emails
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.Mail.Types
( MailServer (..)
)
where

import IHP.Prelude
import Network.Socket (PortNumber) 

-- | Configuration for a mailer used by IHP
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
    -- | Uses a generic SMTP for sending emails
    | SMTP { host :: String
           , port :: PortNumber
           -- (Username,Password) combination
           , credentials :: Maybe (String, String)}
