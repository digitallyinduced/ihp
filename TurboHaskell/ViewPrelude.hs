{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module TurboHaskell.ViewPrelude (
    module TurboHaskell.Prelude,
    module TurboHaskell.View.TimeAgo,
    stringValue,
    isActivePath,
    module TurboHaskell.View.Form,
    viewContext,
    hsx,
    toHtml,
    isActivePathOrSub,
    preEscapedToHtml,
    module TurboHaskell.View.Modal,
    module TurboHaskell.ValidationSupport,
    addStyle,
    css,
    pathTo,
    module TurboHaskell.ViewSupport,
    module TurboHaskell.ModelSupport,
    (!),
    module Data.Data,
    param,
    fetch,
    query
) where

import TurboHaskell.Prelude
import           TurboHaskell.ViewErrorMessages
import           TurboHaskell.ViewSupport
import Text.Blaze (preEscapedText, stringValue, text, (!))
import Text.Blaze.Html5 (preEscapedToHtml)
import TurboHaskell.View.Form
import TurboHaskell.HtmlSupport.QQ (hsx)
import TurboHaskell.HtmlSupport.ToHtml
import TurboHaskell.View.TimeAgo
import TurboHaskell.View.Modal
import TurboHaskell.ValidationSupport
import TurboHaskell.Controller.RequestContext
import TurboHaskell.RouterSupport
import TurboHaskell.ModelSupport
import Data.Data
