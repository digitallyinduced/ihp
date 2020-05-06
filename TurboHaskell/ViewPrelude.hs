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
    module Data.List.Split,
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
import qualified Network.Wai
import           Text.Blaze                   (Attribute, dataAttribute, preEscapedText, stringValue, text)
import           Text.Blaze.Html5             (preEscapedToHtml, a, body, button, div, docTypeHtml, footer, form, h1, h2, h3, h4, h5, h6, head, hr, html, iframe, img, input,
                                               label, li, link, meta, nav, ol, p, script, small, span, table, tbody, td, th, thead, title, tr, ul, pre, code, select, option, (!))
import qualified Text.Blaze.Html5             as Html5
import           Text.Blaze.Html5.Attributes  (action, autocomplete, autofocus, charset, class_, selected, checked, content, href, httpEquiv, id, lang, method, name, onclick, onload,
                                               placeholder, rel, src, style, type_, value)
import qualified Text.Blaze.Html5.Attributes  as A
import TurboHaskell.View.Form
import TurboHaskell.HtmlSupport.QQ (hsx)
import TurboHaskell.HtmlSupport.ToHtml
import TurboHaskell.View.TimeAgo
import Data.List.Split (chunksOf)
import TurboHaskell.View.Modal
import TurboHaskell.ValidationSupport
import TurboHaskell.Controller.RequestContext
import TurboHaskell.RouterSupport
import TurboHaskell.ModelSupport
import Data.Data
import GHC.TypeLits as T
import qualified Data.ByteString as ByteString
