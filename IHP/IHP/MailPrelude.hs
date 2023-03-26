{-|
Module: IHP.MailPrelude
Description: Prelude for email views
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.MailPrelude
( module IHP.Mail
, module Network.Mail.Mime
, module IHP.ViewPrelude
, module IHP.Mail.Types
) where

import IHP.Mail
import IHP.Mail.Types
import Network.Mail.Mime
import IHP.ViewPrelude hiding (html)