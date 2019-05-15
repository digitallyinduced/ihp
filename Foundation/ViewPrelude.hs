{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Foundation.ViewPrelude (
    HtmlWithContext,

    ($), (!), forM_, mempty,

    module Foundation.HaskellSupport,
    module ClassyPrelude,
    module Foundation.View.TimeAgo,
    (<>),
    Show (show),
    stringValue,

    cs,

    isActivePath,
    when,
    Int,

    Maybe (..),
    Text,

    module Foundation.View.Form,
    viewContext,
    hsx,
    timeAgo,
    toHtml,
    tshow,
    UUID,
    def,
    Bool (..),
    (==),
    find, isJust,
    module GHC.OverloadedLabels,
    module GHC.Records,
    module Data.List.Split,
    isActivePathOrSub,
    plain,
    preEscapedToHtml,
    module Foundation.View.Modal,
    classes,
    module Foundation.ModelSupport,
    module Foundation.ValidationSupport,
    addStyle,
    css,
    pathTo,
    (:>)(..),
    module Foundation.ViewSupport
) where

import ClassyPrelude
import           Control.Monad                (when)
import           Data.String.Conversions      (ConvertibleStrings (convertString), cs)
import           Data.Text                    (Text, intercalate)
import           Foundation.HaskellSupport
import qualified Foundation.ModelSupport
import Foundation.ModelSupport (Include, Include', inputValue)
import           Foundation.ViewErrorMessages
import           Foundation.ViewSupport
import qualified Network.Wai
import           Text.Blaze                   (Attribute, dataAttribute, preEscapedText, stringValue, text)
import           Text.Blaze.Html5             (preEscapedToHtml, a, body, button, div, docTypeHtml, footer, form, h1, h2, h3, h4, h5, h6, head, hr, html, iframe, img, input,
                                               label, li, link, meta, nav, ol, p, script, small, span, table, tbody, td, th, thead, title, tr, ul, pre, code, select, option)
import           Text.Blaze.Html5             ((!))
import qualified Text.Blaze.Html5             as Html5
import           Text.Blaze.Html5.Attributes  (action, autocomplete, autofocus, charset, class_, selected, checked, content, href, httpEquiv, id, lang, method, name, onclick, onload,
                                               placeholder, rel, src, style, type_, value)
import qualified Text.Blaze.Html5.Attributes  as A
import Foundation.View.Form
import Foundation.View.ConvertibleStrings ()
import Foundation.HtmlSupport.QQ (hsx)
import qualified Data.Time.Format
import Data.Time.Clock (UTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Unsafe.Coerce
import Foundation.HtmlSupport.ToHtml
import Data.UUID (UUID)
import Data.Default (def)
import Foundation.View.TimeAgo
import GHC.OverloadedLabels (fromLabel)
import GHC.Records (HasField, getField)
import Data.List.Split (chunksOf)
import qualified Data.String.Interpolate
import Foundation.View.Modal
import Foundation.ValidationSupport
import Foundation.Controller.RequestContext
import Foundation.RouterSupport

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
        currentPath == (cs $ pathTo route)

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
addStyle style = Html5.style $ (preEscapedText $ cs style)
