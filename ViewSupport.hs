{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Foundation.ViewSupport (ViewContext (ViewContext), Html, Html', ToAttributeValue (toAttributeValue), classes) where

import ClassyPrelude
import qualified Text.Blaze
import qualified Text.Blaze.Html5 as Html5
import Apps.Web.View.Context
import Foundation.HaskellSupport

type Html = (?viewContext :: ViewContext) => Html'
type Html' = Html5.Html

class ToAttributeValue a where
    toAttributeValue :: a -> Html5.AttributeValue

instance ToAttributeValue Html5.AttributeValue where
    {-# INLINE toAttributeValue #-}
    toAttributeValue = id

instance ToAttributeValue String where
    {-# INLINE toAttributeValue #-}
    toAttributeValue = Html5.stringValue

classes :: [(Text, Bool)] -> Text
classes classNameBoolPairs = classNameBoolPairs |> filter snd |> map fst |> intercalate " "
