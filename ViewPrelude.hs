{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Foundation.ViewPrelude (
    Html,

    ($), (!), forM_, mempty,

    module Foundation.HaskellSupport,
    module ClassyPrelude,
    module Foundation.View.TimeAgo,
    module Model.Generated.Types,
    (<>),
    Show (show),
    stringValue,

    cs,

    isActivePath,
    when,
    Int,

    Maybe (..),
    Text,

    module UrlGenerator,
    module Foundation.View.Form,
    module Model.Generated.Validators,
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
    ViewContext,
    module Foundation.UrlGeneratorSupport,
    module GHC.OverloadedLabels,
    module GHC.Records,
    module Data.List.Split
) where

import Model.Generated.Types
import ClassyPrelude
import           Control.Monad                (when)
import           Data.String.Conversions      (ConvertibleStrings (convertString), cs)
import           Data.Text                    (Text, intercalate)
import           Foundation.HaskellSupport
import qualified Foundation.ModelSupport
import           Foundation.ViewErrorMessages
import           Foundation.ViewSupport
import qualified Network.Wai
import           Text.Blaze                   (Attribute, dataAttribute, preEscapedText, stringValue, text)
import           Text.Blaze.Html5             (a, body, button, div, docTypeHtml, footer, form, h1, h2, h3, h4, h5, h6, head, hr, html, iframe, img, input,
                                               label, li, link, meta, nav, ol, p, script, small, span, table, tbody, td, th, thead, title, tr, ul, pre, code, select, option)
import           Text.Blaze.Html5             ((!))
import qualified Text.Blaze.Html5             as Html5
import           Text.Blaze.Html5.Attributes  (action, autocomplete, autofocus, charset, class_, selected, checked, content, href, httpEquiv, id, lang, method, name, onclick, onload,
                                               placeholder, rel, src, style, type_, value)
import qualified Text.Blaze.Html5.Attributes  as A
import           UrlGenerator
import qualified View.Context
import Foundation.View.Form
import Foundation.View.ConvertibleStrings ()
import Model.Generated.Validators
import Foundation.HtmlSupport.QQ (hsx)
import qualified Data.Time.Format
import Data.Time.Clock (UTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Unsafe.Coerce
import Foundation.HtmlSupport.ToHtml
import Data.UUID (UUID)
import Data.Default (def)
import Foundation.View.TimeAgo
import Foundation.UrlGeneratorSupport
import GHC.OverloadedLabels (fromLabel)
import GHC.Records (HasField)
import Data.List.Split (chunksOf)

onClick = onclick
onLoad = onload


isActivePath :: (?viewContext :: View.Context.ViewContext) => Text -> ClassyPrelude.Bool
isActivePath path =
    let
        currentPath = Network.Wai.rawPathInfo (View.Context.request ?viewContext)
    in
        currentPath == (cs path)

viewContext :: (?viewContext :: View.Context.ViewContext) => View.Context.ViewContext
viewContext = ?viewContext
