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
import Network.Mail.Mime
import qualified Network.Mail.Mime.SES as Mailer
import Text.Blaze.Html5 (Html)
import qualified Text.Blaze.Html.Renderer.Text as Blaze

-- | Configuration for a mailer used by IHP
data MailServer =
    -- | Uses AWS SES for sending emails. Highly recommended in production
    SES { accessKey :: ByteString
        , secretKey :: ByteString
        --  | E.g. @"us-east-1"@ or @"eu-west-1"@
        , region :: Text }
    -- | Uses the local Sendmail binary for sending emails
    | Sendmail
