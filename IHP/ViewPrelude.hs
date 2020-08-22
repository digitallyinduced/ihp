{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module IHP.ViewPrelude (
    module IHP.Prelude,
    module IHP.View.TimeAgo,
    stringValue,
    module IHP.View.Form,
    hsx,
    toHtml,
    preEscapedToHtml,
    module IHP.View.Modal,
    module IHP.ValidationSupport,
    pathTo,
    module IHP.ViewSupport,
    module IHP.ModelSupport,
    (!),
    module Data.Data,
    module Data.Aeson
) where

import IHP.Prelude
import           IHP.ViewErrorMessages
import           IHP.ViewSupport
import Text.Blaze (preEscapedText, stringValue, text, (!))
import Text.Blaze.Html5 (preEscapedToHtml)
import IHP.View.Form
import IHP.HtmlSupport.QQ (hsx)
import IHP.HtmlSupport.ToHtml
import IHP.View.TimeAgo
import IHP.View.Modal
import IHP.ValidationSupport
import IHP.Controller.RequestContext
import IHP.RouterSupport
import IHP.ModelSupport
import Data.Data
import Data.Aeson