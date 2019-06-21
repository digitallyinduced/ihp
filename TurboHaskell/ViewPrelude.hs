{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module TurboHaskell.ViewPrelude (
    module TurboHaskell.HaskellSupport,
    module ClassyPrelude,
    module TurboHaskell.View.TimeAgo,
    stringValue,

    cs,

    isActivePath,
    module TurboHaskell.View.Form,
    viewContext,
    hsx,
    toHtml,
    UUID,
    def,
    module GHC.OverloadedLabels,
    module GHC.Records,
    module Data.List.Split,
    isActivePathOrSub,
    plain,
    preEscapedToHtml,
    module TurboHaskell.View.Modal,
    classes,
    module TurboHaskell.ValidationSupport,
    addStyle,
    css,
    pathTo,
    (:>)(..),
    module TurboHaskell.ViewSupport,
    module TurboHaskell.ModelSupport,
    (!)
) where

import ClassyPrelude
import           Data.String.Conversions      (ConvertibleStrings (convertString), cs)
import           Data.Text                    (Text, intercalate)
import           TurboHaskell.HaskellSupport
import qualified TurboHaskell.ModelSupport
import TurboHaskell.ModelSupport (Include, Include', inputValue)
import           TurboHaskell.ViewErrorMessages
import           TurboHaskell.ViewSupport
import qualified Network.Wai
import           Text.Blaze                   (Attribute, dataAttribute, preEscapedText, stringValue, text)
import           Text.Blaze.Html5             (preEscapedToHtml, a, body, button, div, docTypeHtml, footer, form, h1, h2, h3, h4, h5, h6, head, hr, html, iframe, img, input,
                                               label, li, link, meta, nav, ol, p, script, small, span, table, tbody, td, th, thead, title, tr, ul, pre, code, select, option)
import           Text.Blaze.Html5             ((!))
import qualified Text.Blaze.Html5             as Html5
import           Text.Blaze.Html5.Attributes  (action, autocomplete, autofocus, charset, class_, selected, checked, content, href, httpEquiv, id, lang, method, name, onclick, onload,
                                               placeholder, rel, src, style, type_, value)
import qualified Text.Blaze.Html5.Attributes  as A
import TurboHaskell.View.Form
import TurboHaskell.View.ConvertibleStrings ()
import TurboHaskell.HtmlSupport.QQ (hsx)
import qualified Data.Time.Format
import Data.Time.Clock (UTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Unsafe.Coerce
import TurboHaskell.HtmlSupport.ToHtml
import Data.UUID (UUID)
import Data.Default (def)
import TurboHaskell.View.TimeAgo
import GHC.OverloadedLabels (fromLabel)
import GHC.Records (HasField, getField)
import Data.List.Split (chunksOf)
import qualified Data.String.Interpolate
import TurboHaskell.View.Modal
import TurboHaskell.ValidationSupport
import TurboHaskell.Controller.RequestContext
import TurboHaskell.RouterSupport
import TurboHaskell.ModelSupport

plain = Data.String.Interpolate.i
css = Data.String.Interpolate.i

onClick = onclick
onLoad = onload

{-# INLINE theRequest #-}
theRequest :: (?viewContext :: viewContext, HasField "requestContext" viewContext RequestContext) => Network.Wai.Request
theRequest = 
    let
        requestContext = getField @"requestContext" ?viewContext
        request = getField @"request" requestContext
    in request


isActivePath :: (?viewContext :: viewContext, HasField "requestContext" viewContext RequestContext, HasPath controller) => controller -> ClassyPrelude.Bool
isActivePath route =
    let 
        currentPath = Network.Wai.rawPathInfo theRequest
    in
        currentPath == cs (pathTo route)

isActivePathOrSub :: (?viewContext :: viewContext, HasField "requestContext" viewContext RequestContext, HasPath controller) => controller -> ClassyPrelude.Bool
isActivePathOrSub route =
    let
        currentPath = Network.Wai.rawPathInfo theRequest
    in
        (cs $ pathTo route) `isPrefixOf` currentPath

{-# INLINE viewContext #-}
viewContext :: (?viewContext :: viewContext) => viewContext
viewContext = ?viewContext

{-# INLINE addStyle #-}
addStyle :: (ConvertibleStrings string Text) => string -> Html5.Markup
addStyle style = Html5.style $ preEscapedText (cs style)
