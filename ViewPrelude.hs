{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Foundation.ViewPrelude (
    Html, div, span, p, a, href, nav, h1, h2, h3, h4, h5, ul, ol, id, li, head, meta, title, link, docTypeHtml, script, body, form, input, select, option, label, button, text, value, hr, footer, table, thead, tr, th, tbody, td, onClick, preEscapedText, iframe, placeholder, autofocus, autocomplete, img, httpEquiv, content, small, dataAttribute, h6, pre, code,

    src, class_, lang, rel, charset, type_, method, action, name, style, selected, checked,

    ($), (!), forM_, mempty,

    module Foundation.HaskellSupport,
    (<>),
    Show (show),
    stringValue,

    StyleRule (BackgroundColor, FontSize),

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
    timeAgo
) where

import           ClassyPrelude                (Int, Maybe (..), Show (show), String, Text, fmap, forM_, fromString, mempty, ($), (.), (<>), (==))
import qualified ClassyPrelude
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
import           Text.Blaze.Html5.Attributes  (action, autocomplete, autofocus, charset, class_, selected, checked, content, href, httpEquiv, id, lang, method, name, onclick,
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

type Style = [StyleRule]
data StyleRule = BackgroundColor Text | FontSize Text

style2 :: Style -> Attribute
style2 theStyle = A.style (stringValue (cs (intercalate "; " (fmap compile theStyle))))
    where
        compile :: StyleRule -> Text
        compile (BackgroundColor color) = "background-color: " <> color
        compile (FontSize size)         = "font-size: " <> size

onClick = onclick


isActivePath :: (?viewContext :: View.Context.ViewContext) => Text -> ClassyPrelude.Bool
isActivePath path =
    let
        currentPath = Network.Wai.rawPathInfo (View.Context.request ?viewContext)
    in
        currentPath == (cs path)

viewContext :: (?viewContext :: View.Context.ViewContext) => View.Context.ViewContext
viewContext = ?viewContext


timeAgo :: ClassyPrelude.UTCTime -> Html5.Html
timeAgo dateTime = span ! A.datetime (cs $ formatDateTime dateTime) $ cs (formatDateTime dateTime)
    where
        formatDateTime time = iso8601Show (unsafeCoerce time :: UTCTime)