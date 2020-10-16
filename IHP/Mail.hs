{-|
Module: IHP.Mail
Description: Send Emails
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.Mail
( MailServer (..)
, BuildMail (..)
, sendMail
, sendWithMailServer
)
where

import IHP.Prelude
import IHP.ControllerSupport
import IHP.Mail.Types

import           Network.Mail.Mime
import qualified Network.Mail.Mime.SES                as Mailer
import qualified Network.HTTP.Client
import qualified Network.HTTP.Client.TLS
import Text.Blaze.Html5 (Html)
import qualified Text.Blaze.Html.Renderer.Text as Blaze
import qualified IHP.FrameworkConfig as Config

buildMail :: BuildMail mail => mail -> IO Mail
buildMail mail = let ?mail = mail in simpleMail (to mail) from subject (cs $ text mail) (html mail |> Blaze.renderHtml) []

-- | Sends an email
--
-- Uses the mail server provided in the controller context, configured in Config/Config.hs
sendMail :: (Config.FrameworkConfig, BuildMail mail) => mail -> IO ()
sendMail mail = buildMail mail >>= sendWithMailServer Config.mailServer

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
