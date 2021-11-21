{-|
Module: IHP.Mail
Description: Send Emails
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.Mail
( MailServer (..)
, BuildMail (..)
, SMTPEncryption ( ..)
, sendMail
, sendWithMailServer
)
where

import IHP.Prelude
import IHP.Mail.Types
import IHP.FrameworkConfig

import           Network.Mail.Mime
import qualified Network.Mail.Mime.SES                as Mailer
import qualified Network.Mail.SMTP                    as SMTP
import qualified Network.HTTP.Client
import qualified Network.HTTP.Client.TLS
import Text.Blaze.Html5 (Html)
import qualified Text.Blaze.Html.Renderer.Text as Blaze
import qualified Data.Text as Text
import Data.Maybe

buildMail :: (BuildMail mail, ?context :: context, ConfigProvider context) => mail -> Mail
buildMail mail = let ?mail = mail in simpleMailInMemory (to mail) from subject (cs $ text mail) (html mail |> Blaze.renderHtml) attachments'
    where
        attachments' = mail
                |> attachments
                |> map (\MailAttachment { name, content, contentType } -> (contentType, name, content))

-- | Sends an email
--
-- Uses the mail server provided in the controller context, configured in Config/Config.hs
sendMail :: (BuildMail mail, ?context :: context, ConfigProvider context) => mail -> IO ()
sendMail mail = sendWithMailServer (fromConfig mailServer) (buildMail mail)

sendWithMailServer :: MailServer -> Mail -> IO ()
sendWithMailServer SES { .. } mail = do
    manager <- Network.HTTP.Client.newManager Network.HTTP.Client.TLS.tlsManagerSettings
    let ses = Mailer.SES {
            Mailer.sesFrom = cs $ addressEmail (mailFrom mail),
            Mailer.sesTo = map (cs . addressEmail) (mailTo mail),
            Mailer.sesAccessKey = accessKey,
            Mailer.sesSecretKey = secretKey,
            Mailer.sesSessionToken = Nothing,
            Mailer.sesRegion = region
        }
    Mailer.renderSendMailSES manager ses mail

sendWithMailServer SendGrid { .. } mail = do
    let mail' = if isJust category then mail {mailHeaders = ("X-SMTPAPI","{\"category\": \"" ++ (fromJust category) ++ "\"}") : headers} else mail
    SMTP.sendMailWithLoginSTARTTLS' "smtp.sendgrid.net" 587 "apikey" (Text.unpack apiKey) mail'
    where headers = mailHeaders mail

sendWithMailServer IHP.Mail.Types.SMTP { .. } mail
    | isNothing credentials =
          case encryption of
              Unencrypted -> SMTP.sendMail' host port mail
              TLS -> SMTP.sendMailTLS' host port mail
              STARTTLS -> SMTP.sendMailSTARTTLS' host port mail
    | otherwise =
          case encryption of
              Unencrypted -> SMTP.sendMailWithLogin' host port (fst creds) (snd creds) mail
              TLS -> SMTP.sendMailWithLoginTLS' host port (fst creds) (snd creds) mail
              STARTTLS -> SMTP.sendMailWithLoginSTARTTLS' host port (fst creds) (snd creds) mail
    where creds = fromJust credentials

sendWithMailServer Sendmail mail = do
    message <- renderMail' mail
    sendmail message

class BuildMail mail where
    -- | You can use @?mail@ to make this dynamic based on the given entity
    subject :: (?mail :: mail) => Text

    -- | The email receiver
    --
    -- __Example:__
    --
    -- > to ConfirmationMail { .. } = Address { addressName = Just (get #name user), addressEmail = get #email user }
    --
    -- __Example:__ Send all emails to a fixed email address while in development mode
    --
    -- > to CreateAccountMail { .. } = Address
    -- >     { addressName = Just (fullName admin)
    -- >     , addressEmail =
    -- >         if isDevelopment then
    -- >             "staging@example.com"
    -- >         else
    -- >             get #email admin
    -- >     }
    --
    to :: (?context :: context, ConfigProvider context) => mail -> Address

    -- | Your sender address
    --
    -- __Example:__
    --
    -- > from = Address { addressName = "Acme Inc.", addressEmail = "hi@example.com" }
    --
    from :: (?mail :: mail, ?context :: context, ConfigProvider context) => Address

    -- | Similiar to a normal html view, HSX can be used here
    html :: (?context :: context, ConfigProvider context) => mail -> Html

    -- | When no plain text version of the email is specified it falls back to using the html version but striping out all the html tags
    text :: (?context :: context, ConfigProvider context) => mail -> Text
    text mail = stripTags (cs $ Blaze.renderHtml (html mail))

    -- | Optional, mail attachments
    --
    -- __Example:__
    --
    -- > attachments = [ MailAttachment { name = "attached_file.xml", contentType = "application/xml", content = "<xml><hello/></xml>" } ]
    --
    attachments :: (?context :: context, ConfigProvider context) => mail -> [MailAttachment]
    attachments mail = []
