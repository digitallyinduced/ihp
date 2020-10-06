{-|
Module: IHP.Mail.Types
Description: Types for emails
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.Mail.Types
( MailServer (..)
, BuildMail (..)
)
where

import IHP.Prelude
import Network.Mail.Mime
import qualified Network.Mail.Mime.SES as Mailer
import Text.Blaze.Html5 (Html)
import qualified Text.Blaze.Html.Renderer.Text as Blaze

-- | Configuration for a mailer used by IHP. Currently we only support AWS SES
data MailServer =
    SES
    { accessKey :: ByteString
    , secretKey :: ByteString
    --  | E.g. @"us-east-1"@ or @"eu-west-1"@
    , region :: Text }


class BuildMail mail where
    -- | You can use @?mail@ to make this dynamic based on the given entity
    subject :: (?mail :: mail) => Text
    
    -- | The email receiver
    --
    -- __Example:__
    -- > to ConfirmationMail { .. } = Address { addressName = Just (get #name user), addressEmail = get #email user }
    to :: mail -> Address

    -- | Your sender address
    from :: (?mail :: mail) => Address

    -- | Similiar to a normal html view, HSX can be used here
    html :: mail -> Html

    -- | When no plain text version of the email is specified it falls back to using the html version but striping out all the html tags
    text :: mail -> Text
    text mail = stripTags (cs $ Blaze.renderHtml (html mail))