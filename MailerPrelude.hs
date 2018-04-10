module Foundation.MailerPrelude (
        module ClassyPrelude,
        module Network.Mail.Mime,
        module Model.Generated.Types,
        module Foundation.HaskellSupport,
        module UrlGenerator,
        cs,
        plain,
        baseUrl
    ) where

import           ClassyPrelude
import           Config                    (baseUrl)
import           Data.String.Conversions   (cs)
import qualified Data.String.Interpolate
import           Foundation.HaskellSupport
import           Model.Generated.Types
import           Network.Mail.Mime
import           UrlGenerator

plain = Data.String.Interpolate.i
