module TurboHaskell.MailerPrelude (
        module ClassyPrelude,
        module Network.Mail.Mime,
        module Generated.Types,
        module TurboHaskell.HaskellSupport,
        cs,
        plain,
    ) where

import           ClassyPrelude
import           Data.String.Conversions   (cs)
import qualified Data.String.Interpolate
import           TurboHaskell.HaskellSupport
import           Generated.Types
import           Network.Mail.Mime

plain = Data.String.Interpolate.i
